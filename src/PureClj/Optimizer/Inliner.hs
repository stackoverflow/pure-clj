module PureClj.Optimizer.Inliner where

import Prelude.Compat

import qualified PureClj.Constants as C

import Control.Monad.Supply.Class
import Data.Monoid ((<>))
import Data.String (IsString)
import Data.Text (Text)

import PureClj.AST
import PureClj.Optimizer.Common

shouldInline :: Clj -> Bool
shouldInline (CljVar Nothing _) = True
shouldInline (CljNumericLiteral _) = True
shouldInline (CljStringLiteral _) = True
shouldInline (CljKeywordLiteral _) = True
shouldInline (CljCharLiteral _) = True
shouldInline (CljBooleanLiteral _) = True
shouldInline (CljArrayIndexer index val) = shouldInline index && shouldInline val
shouldInline (CljAccessor _ val) = shouldInline val
-- | keywords as functions = acessor, therefore safe if the parameter is safe
shouldInline (CljApp (CljKeywordLiteral _) [par]) = shouldInline par
-- | constructors are safe to inline if they parameters are safe
shouldInline (CljApp (CljApp (CljKeywordLiteral "new") [var]) pars) =
  shouldInline var && all shouldInline pars
shouldInline _ = False

shouldInlineDef :: Clj -> Bool
shouldInlineDef (CljDef LetDef _ val) = shouldInline val
shouldInlineDef _ = False

etaConvert :: Clj -> Clj
etaConvert = everywhere convert
  where
    convert :: Clj -> Clj
    convert (CljFunction _ [p] [(CljApp (CljVar ns v) [(CljVar Nothing p2)])]) | p == p2 =
      CljVar ns v
    convert other = other

inlineVariables :: Clj -> Clj
inlineVariables = everywhere $ removeFromLet go
  where
    go :: [Clj] -> [Clj] -> ([Clj], [Clj])
    go [] clj = ([], clj)
    go (CljDef LetDef var val : sts) clj
      | shouldInline val && not (any (isReassigned var) (sts ++ clj)) =
        let sts' = map (replaceIdent var val) sts
            clj' = map (replaceIdent var val) clj
        in go sts' clj'
    go (s:sts) clj =
      let (defs', clj') = go sts clj
      in (s:defs', clj')

inlineCommonValues :: Clj -> Clj
inlineCommonValues = everywhere convert
  where
  convert :: Clj -> Clj
  convert (CljVar Nothing "undefined") = CljVar Nothing C.nil
  convert (CljLet defs [CljLet defs' body]) = CljLet (defs ++ defs') body
  convert (CljApp fn [dict])
    | isDict' [semiringNumber, semiringInt] dict && isDict fnZero fn = CljNumericLiteral (Left 0)
    | isDict' [semiringNumber, semiringInt] dict && isDict fnOne fn = CljNumericLiteral (Left 1)
    | isDict boundedBoolean dict && isDict fnBottom fn = CljBooleanLiteral False
    | isDict boundedBoolean dict && isDict fnTop fn = CljBooleanLiteral True
  convert other = other
  fnZero = (C.dataSemiring, C.zero)
  fnOne = (C.dataSemiring, C.one)
  fnBottom = (C.dataBounded, C.bottom)
  fnTop = (C.dataBounded, C.top)

inlineCommonOps :: Clj -> Clj
inlineCommonOps = everywhereTopDown $ applyAll $
  [ binary semiringNumber opAdd Add
  , binary semiringNumber opMul Multiply

  , binary semiringInt opAdd Add
  , binary semiringInt opMul Multiply

  , binary ringNumber opSub Subtract
  , unary ringNumber opNegate Negate

  , binary ringInt opSub Subtract
  , unary ringInt opNegate Negate

  , binary euclideanRingNumber opDiv Divide

  -- have to comment this because the way equals (=) works in Clojure
  -- it short-circuits to true if the objects' reference are equals so
  -- (= Double/NaN Double/NaN) -> false
  -- but
  -- (def nan Double/NaN)
  -- (= nan nan) -> true
  -- , binary eqNumber opEq Equal
  -- , binary eqNumber opNotEq NotEqual
  , binary eqInt opEq Equal
  , binary eqInt opNotEq NotEqual
  , binary eqString opEq Equal
  , binary eqString opNotEq NotEqual
  , binary eqChar opEq Equal
  , binary eqChar opNotEq NotEqual
  , binary eqBoolean opEq Equal
  , binary eqBoolean opNotEq NotEqual

  , binary ordBoolean opLessThan LessThan
  , binary ordBoolean opLessThanOrEq LessThanOrEqual
  , binary ordBoolean opGreaterThan GreaterThan
  , binary ordBoolean opGreaterThanOrEq GreaterThanOrEqual
  , binary ordChar opLessThan LessThan
  , binary ordChar opLessThanOrEq LessThanOrEqual
  , binary ordChar opGreaterThan GreaterThan
  , binary ordChar opGreaterThanOrEq GreaterThanOrEqual
  , binary ordInt opLessThan LessThan
  , binary ordInt opLessThanOrEq LessThanOrEqual
  , binary ordInt opGreaterThan GreaterThan
  , binary ordInt opGreaterThanOrEq GreaterThanOrEqual
  , binary ordNumber opLessThan LessThan
  , binary ordNumber opLessThanOrEq LessThanOrEqual
  , binary ordNumber opGreaterThan GreaterThan
  , binary ordNumber opGreaterThanOrEq GreaterThanOrEqual
  , binary ordString opLessThan LessThan
  , binary ordString opLessThanOrEq LessThanOrEqual
  , binary ordString opGreaterThan GreaterThan
  , binary ordString opGreaterThanOrEq GreaterThanOrEqual

  , binary semigroupString opAppend StringAppend

  , binary heytingAlgebraBoolean opConj And
  , binary heytingAlgebraBoolean opDisj Or
  , unary heytingAlgebraBoolean opNot Not

  , binary' C.dataIntBits C.or BitOr
  , binary' C.dataIntBits C.and BitAnd
  , binary' C.dataIntBits C.xor BitXor
  , binary' C.dataIntBits C.shl ShiftLeft
  , binary' C.dataIntBits C.shr ShiftRight
  , binary' C.dataIntBits C.zshr UnsignedShiftRight
  , unary' C.dataIntBits C.complement BitwiseNot

  , inlineNonClassFunction (isModFn (C.dataFunction, C.apply)) $ \f x -> CljApp f [x]
  , inlineNonClassFunction (isModFn (C.dataFunction, C.applyFlipped)) $ \x f -> CljApp f [x]
  ]
  where
    binary :: (Text, Text) -> (Text, Text) -> BinaryOperator -> Clj -> Clj
    binary dict fns op = convert where
      convert :: Clj -> Clj
      convert (CljApp (CljApp (CljApp fn [dict']) [x]) [y]) | isDict dict dict' && isDict fns fn =
        CljBinary op [x, y]
      convert other = other
    binary' :: Text -> Text -> BinaryOperator -> Clj -> Clj
    binary' moduleName opString op = convert where
      convert :: Clj -> Clj
      convert (CljApp (CljApp fn [x]) [y]) | isDict (moduleName, opString) fn =
        CljBinary op [x, y]
      convert other = other
    unary :: (Text, Text) -> (Text, Text) -> UnaryOperator -> Clj -> Clj
    unary dicts fns op = convert where
      convert :: Clj -> Clj
      convert (CljApp (CljApp fn [dict']) [x]) | isDict dicts dict' && isDict fns fn =
        CljUnary op x
      convert other = other
    unary' :: Text -> Text -> UnaryOperator -> Clj -> Clj
    unary' moduleName fnName op = convert where
      convert :: Clj -> Clj
      convert (CljApp fn [x]) | isDict (moduleName, fnName) fn = CljUnary op x
      convert other = other
    inlineNonClassFunction :: (Clj -> Bool) -> (Clj -> Clj -> Clj) -> Clj -> Clj
    inlineNonClassFunction p f = convert where
      convert :: Clj -> Clj
      convert (CljApp (CljApp op' [x]) [y]) | p op' = f x y
      convert other = other
    isModFn :: (Text, Text) -> Clj -> Bool
    isModFn (m, op) (CljVar (Just m') op') = m == m' && op == op'
    isModFn _ _ = False

inlineFnComposition :: forall m. MonadSupply m => Clj -> m Clj
inlineFnComposition = everywhereTopDownM convert where
  convert :: Clj -> m Clj
  convert (CljApp (CljApp (CljApp (CljApp fn [dict']) [x]) [y]) [z])
    | isFnCompose dict' fn = return $ CljApp x [CljApp y [z]]
    | isFnComposeFlipped dict' fn = return $ CljApp y [CljApp x [z]]
  convert (CljApp (CljApp (CljApp fn [dict']) [x]) [y])
    | isFnCompose dict' fn = do
        arg <- freshName
        return $ CljFunction Nothing [arg] [(CljApp x [CljApp y [CljVar Nothing arg]])]
    | isFnComposeFlipped dict' fn = do
        arg <- freshName
        return $ CljFunction Nothing [arg] [(CljApp y [CljApp x [CljVar Nothing arg]])]
  convert other = return other
  isFnCompose :: Clj -> Clj -> Bool
  isFnCompose dict' fn = isDict semigroupoidFn dict' && isDict fnCompose fn
  isFnComposeFlipped :: Clj -> Clj -> Bool
  isFnComposeFlipped dict' fn = isDict semigroupoidFn dict' && isDict fnComposeFlipped fn
  fnCompose :: forall a b. (IsString a, IsString b) => (a, b)
  fnCompose = (C.controlSemigroupoid, C.compose)
  fnComposeFlipped :: forall a b. (IsString a, IsString b) => (a, b)
  fnComposeFlipped = (C.controlSemigroupoid, C.composeFlipped)

inlineUnsafeCoerce :: Clj -> Clj
inlineUnsafeCoerce = everywhereTopDown convert where
  convert (CljApp (CljVar (Just unsafeCoerce) unsafeCoerceFn) [ comp ])
    | unsafeCoerceFn == C.unsafeCoerceFn && unsafeCoerce == C.unsafeCoerce
    = comp
  convert other = other

inlineUnsafePartial :: Clj -> Clj
inlineUnsafePartial = everywhereTopDown convert where
  convert (CljApp (CljVar (Just partialUnsafe) unsafePartial) [ comp ])
    | unsafePartial == C.unsafePartial && partialUnsafe == C.partialUnsafe
    = CljApp comp [ CljVar Nothing C.nil ]
  convert other = other



-- | work around Clojure lack of recursive lets
fixLets :: Clj -> Clj
fixLets = everywhere fix
  where
    fix :: Clj -> Clj
    fix (CljDef LetDef var (CljFunction Nothing pars [body])) | isUsed var body && not (var `elem` pars) =
      CljDef LetDef var (CljFunction (Just var) pars [body])
    fix (CljLet defs [v]) = CljLet (go defs) [v]
    fix x = x
    go :: [Clj] -> [Clj]
    go [] = []
    go ((CljDef LetDef var clj) : defs)
      | isUsed var clj && not (isUsedInFunction var clj) && shouldAtomize clj
      = let atom = var <> "$atom" in
          [ CljDef LetDef atom (CljApp (var' "atom") [(var' C.nil)])
          , (CljDef LetDef var (replaceIdent var (var' $ "@" <> atom) clj))
          , CljDef LetDef var (CljApp (var' "reset!") [var' atom, var' var])]
          ++ go defs
    go others = others
    shouldAtomize :: Clj -> Bool
    shouldAtomize (CljFunction _ _ _) = False
    shouldAtomize _ = True
    var' :: Text -> Clj
    var' x = CljVar Nothing x

-- | simplify `cond`s moving to `if`s, `and`s removing useless clauses
-- | and `str`s (string append)
simplifyConds :: Clj -> Clj
simplifyConds = everywhere go where
  go :: Clj -> Clj
  go (CljBinary And [clause]) = clause
  go (CljBinary And clauses) | any isTrue clauses = CljBinary And $ filter (not . isTrue) clauses
  go (CljBinary And clauses) | any isAnd clauses = CljBinary And $ replaceAnds clauses
    where
      replaceAnds :: [Clj] -> [Clj]
      replaceAnds [] = []
      replaceAnds ((CljBinary And exprs):rest) = exprs ++ replaceAnds rest
      replaceAnds (x:rest) = x : replaceAnds rest
  go (CljBinary StringAppend strs) | any isStr strs = CljBinary StringAppend $ replaceStr strs
    where
      replaceStr :: [Clj] -> [Clj]
      replaceStr [] = []
      replaceStr ((CljBinary StringAppend pars):rest) = pars ++ replaceStr rest
      replaceStr (x:rest) = x : replaceStr rest
  go (CljCond [(CljBooleanLiteral True, expr)] _) = expr
  go (CljCond [(check, expr), (CljBooleanLiteral True, expr2)] _) = CljIf check expr expr2
  go other = other
  isTrue :: Clj -> Bool
  isTrue (CljBooleanLiteral True) = True
  isTrue _ = False
  isAnd :: Clj -> Bool
  isAnd (CljBinary And _) = True
  isAnd _ = False
  isStr :: Clj -> Bool
  isStr (CljBinary StringAppend _) = True
  isStr _ = False

ringNumber :: forall a b. (IsString a, IsString b) => (a, b)
ringNumber = (C.dataRing, C.ringNumber)

ringInt :: forall a b. (IsString a, IsString b) => (a, b)
ringInt = (C.dataRing, C.ringInt)

semiringNumber :: forall a b. (IsString a, IsString b) => (a, b)
semiringNumber = (C.dataSemiring, C.semiringNumber)

semiringInt :: forall a b. (IsString a, IsString b) => (a, b)
semiringInt = (C.dataSemiring, C.semiringInt)

euclideanRingNumber :: forall a b. (IsString a, IsString b) => (a, b)
euclideanRingNumber = (C.dataEuclideanRing, C.euclideanRingNumber)

semigroupString :: forall a b. (IsString a, IsString b) => (a, b)
semigroupString = (C.dataSemigroup, C.semigroupString)

eqNumber :: forall a b. (IsString a, IsString b) => (a, b)
eqNumber = (C.dataEq, C.eqNumber)

eqInt :: forall a b. (IsString a, IsString b) => (a, b)
eqInt = (C.dataEq, C.eqInt)

eqString :: forall a b. (IsString a, IsString b) => (a, b)
eqString = (C.dataEq, C.eqString)

eqChar :: forall a b. (IsString a, IsString b) => (a, b)
eqChar = (C.dataEq, C.eqChar)

eqBoolean :: forall a b. (IsString a, IsString b) => (a, b)
eqBoolean = (C.dataEq, C.eqBoolean)

opAdd :: forall a b. (IsString a, IsString b) => (a, b)
opAdd = (C.dataSemiring, C.add)

opMul :: forall a b. (IsString a, IsString b) => (a, b)
opMul = (C.dataSemiring, C.mul)

opSub :: forall a b. (IsString a, IsString b) => (a, b)
opSub = (C.dataRing, C.sub)

opNegate :: forall a b. (IsString a, IsString b) => (a, b)
opNegate = (C.dataRing, C.negate)

opDiv :: forall a b. (IsString a, IsString b) => (a, b)
opDiv = (C.dataEuclideanRing, C.div)

opEq :: forall a b. (IsString a, IsString b) => (a, b)
opEq = (C.dataEq, C.eq)

opNotEq :: forall a b. (IsString a, IsString b) => (a, b)
opNotEq = (C.dataEq, C.notEq)

opAppend :: forall a b. (IsString a, IsString b) => (a, b)
opAppend = (C.dataSemigroup, C.append)

ordBoolean :: forall a b. (IsString a, IsString b) => (a, b)
ordBoolean = (C.dataOrd, C.ordBoolean)

ordNumber :: forall a b. (IsString a, IsString b) => (a, b)
ordNumber = (C.dataOrd, C.ordNumber)

ordInt :: forall a b. (IsString a, IsString b) => (a, b)
ordInt = (C.dataOrd, C.ordInt)

ordString :: forall a b. (IsString a, IsString b) => (a, b)
ordString = (C.dataOrd, C.ordString)

ordChar :: forall a b. (IsString a, IsString b) => (a, b)
ordChar = (C.dataOrd, C.ordChar)

opLessThan :: forall a b. (IsString a, IsString b) => (a, b)
opLessThan = (C.dataOrd, C.lessThan)

opLessThanOrEq :: forall a b. (IsString a, IsString b) => (a, b)
opLessThanOrEq = (C.dataOrd, C.lessThanOrEq)

opGreaterThan :: forall a b. (IsString a, IsString b) => (a, b)
opGreaterThan = (C.dataOrd, C.greaterThan)

opGreaterThanOrEq :: forall a b. (IsString a, IsString b) => (a, b)
opGreaterThanOrEq = (C.dataOrd, C.greaterThanOrEq)

heytingAlgebraBoolean :: forall a b. (IsString a, IsString b) => (a, b)
heytingAlgebraBoolean = (C.dataHeytingAlgebra, C.heytingAlgebraBoolean)

opConj :: forall a b. (IsString a, IsString b) => (a, b)
opConj = (C.dataHeytingAlgebra, C.conj)

opDisj :: forall a b. (IsString a, IsString b) => (a, b)
opDisj = (C.dataHeytingAlgebra, C.disj)

opNot :: forall a b. (IsString a, IsString b) => (a, b)
opNot = (C.dataHeytingAlgebra, C.not)

boundedBoolean :: forall a b. (IsString a, IsString b) => (a, b)
boundedBoolean = (C.dataBounded, C.boundedBoolean)

semigroupoidFn :: forall a b. (IsString a, IsString b) => (a, b)
semigroupoidFn = (C.controlSemigroupoid, C.semigroupoidFn)

module PureClj.Constants where

--import Prelude.Compat

import Data.String (IsString)

dataRing :: forall a. (IsString a) => a
dataRing = "Data.Ring"

dataSemiring :: forall a. (IsString a) => a
dataSemiring = "Data.Semiring"

dataEuclideanRing :: forall a. (IsString a) => a
dataEuclideanRing = "Data.EuclideanRing"

dataEq :: forall a. (IsString a) => a
dataEq = "Data.Eq"

dataOrd :: forall a. (IsString a) => a
dataOrd = "Data.Ord"

dataSemigroup :: forall a. (IsString a) => a
dataSemigroup = "Data.Semigroup"

dataHeytingAlgebra :: forall a. (IsString a) => a
dataHeytingAlgebra = "Data.HeytingAlgebra"

dataIntBits :: forall a. (IsString a) => a
dataIntBits = "Data.Int.Bits"

dataBounded :: forall a. (IsString a) => a
dataBounded = "Data.Bounded"

ringNumber :: forall a. (IsString a) => a
ringNumber = "ringNumber"

ringInt :: forall a. (IsString a) => a
ringInt = "ringInt"

semiringNumber :: forall a. (IsString a) => a
semiringNumber = "semiringNumber"

semiringInt :: forall a. (IsString a) => a
semiringInt = "semiringInt"

semigroupString :: forall a. (IsString a) => a
semigroupString = "semigroupString"

euclideanRingNumber :: forall a. (IsString a) => a
euclideanRingNumber = "euclideanRingNumber"

euclideanRingInt :: forall a. (IsString a) => a
euclideanRingInt = "euclideanRingInt"

add :: forall a. (IsString a) => a
add = "add"

sub :: forall a. (IsString a) => a
sub = "sub"

mul :: forall a. (IsString a) => a
mul = "mul"

div :: forall a. (IsString a) => a
div = "div"

negate :: forall a. (IsString a) => a
negate = "negate"

eq :: forall a. (IsString a) => a
eq = "eq"

notEq :: forall a. (IsString a) => a
notEq = "notEq"

eqNumber :: forall a. (IsString a) => a
eqNumber = "eqNumber"

eqInt :: forall a. (IsString a) => a
eqInt = "eqInt"

eqString :: forall a. (IsString a) => a
eqString = "eqString"

eqChar :: forall a. (IsString a) => a
eqChar = "eqChar"

eqBoolean :: forall a. (IsString a) => a
eqBoolean = "eqBoolean"

ordBoolean :: forall a. (IsString a) => a
ordBoolean = "ordBoolean"

ordNumber :: forall a. (IsString a) => a
ordNumber = "ordNumber"

ordInt :: forall a. (IsString a) => a
ordInt = "ordInt"

ordString :: forall a. (IsString a) => a
ordString = "ordString"

ordChar :: forall a. (IsString a) => a
ordChar = "ordChar"

lessThan :: forall a. (IsString a) => a
lessThan = "lessThan"

lessThanOrEq :: forall a. (IsString a) => a
lessThanOrEq = "lessThanOrEq"

greaterThan :: forall a. (IsString a) => a
greaterThan = "greaterThan"

greaterThanOrEq :: forall a. (IsString a) => a
greaterThanOrEq = "greaterThanOrEq"

append :: forall a. (IsString a) => a
append = "append"

boundedBoolean :: forall a. (IsString a) => a
boundedBoolean = "boundedBoolean"

heytingAlgebraBoolean :: forall a. (IsString a) => a
heytingAlgebraBoolean = "heytingAlgebraBoolean"

conj :: forall a. (IsString a) => a
conj = "conj"

disj :: forall a. (IsString a) => a
disj = "disj"

not :: forall a. (IsString a) => a
not = "not"

or :: forall a. (IsString a) => a
or = "or"

and :: forall a. (IsString a) => a
and = "and"

xor :: forall a. (IsString a) => a
xor = "xor"

shl :: forall a. (IsString a) => a
shl = "shl"

shr :: forall a. (IsString a) => a
shr = "shr"

zshr :: forall a. (IsString a) => a
zshr = "zshr"

complement :: forall a. (IsString a) => a
complement = "complement"

zero :: forall a. (IsString a) => a
zero = "zero"

one :: forall a. (IsString a) => a
one = "one"

bottom :: forall a. (IsString a) => a
bottom = "bottom"

top :: forall a. (IsString a) => a
top = "top"

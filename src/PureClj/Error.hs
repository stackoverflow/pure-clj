module PureClj.Error where

import Control.Monad.Error.Class (MonadError(..))

-- | Rethrow an error with a more detailed error message in the case of failure
rethrow :: (MonadError e m) => (e -> e) -> m a -> m a
rethrow f = flip catchError (throwError . f)

module ExtensibleIO where
 import Prelude
 import Control.Monad.Skeleton
 import Data.Extensible
 import Data.Proxy

 -- | Run a subset of 'IO' by handles.
 runIOSH :: RecordOf (Interpreter IO) xs -> Eff xs a -> IO a
 runIOSH handles = deboneBy $ handleEff handles >>> \case
  Return a -> return a
  x :>>= f -> x >>= runIOSH handles . f

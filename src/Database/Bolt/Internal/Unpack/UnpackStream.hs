module Database.Bolt.Internal.Unpack.UnpackStream
    ( UnpackStream (..)
    , UnpackT (..)
    , runUnpackT, popBS, topBS
    ) where

import           Control.Applicative       ((<$>))
import           Control.Monad.Trans.State
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as B (drop, take)
import           Data.Map                  (Map (..))
import           Data.Text                 (Text)

type UnpackT m a = StateT ByteString m a

class UnpackStream a where
  unpack :: Monad m => UnpackT m a

runUnpackT :: (UnpackStream a, Monad m) => ByteString -> m a
runUnpackT = evalStateT unpack

topBS :: Monad m => Int -> UnpackT m ByteString
topBS size = gets (B.take size)

popBS :: Monad m => Int -> UnpackT m ByteString
popBS size = do str <- gets (B.take size)
                modify (B.drop size)
                return str

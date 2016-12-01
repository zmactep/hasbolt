module Database.Bolt.Internal.Unpack.UnpackStream
    ( UnpackStream (..)
    , UnpackST (..)
    , unpackAll, popBS, topBS
    ) where

import           Control.Applicative       ((<$>))
import           Control.Monad.Trans.State
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as B (drop, take)
import           Data.Map                  (Map (..))
import           Data.Text                 (Text)

type UnpackST m a = StateT ByteString m a

class UnpackStream a where
  unpack :: Monad m => UnpackST m a

unpackAll :: (UnpackStream a, Monad m) => ByteString -> m a
unpackAll = (fst <$>) . runStateT unpack

topBS :: Monad m => Int -> UnpackST m ByteString
topBS size = gets (B.take size)

popBS :: Monad m => Int -> UnpackST m ByteString
popBS size = do str <- gets (B.take size)
                modify (B.drop size)
                return str

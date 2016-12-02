module Database.Bolt
    ( BoltActionT (..)
    , connect, run, close
    , NeoValue (..)
    , Node (..), Relationship (..), UnboundRelationship (..), Path (..)
    , BoltCfg (..), Default (..)
    , Pipe
    ) where


import           Control.Monad              (when)
import           Control.Monad.IO.Class     (MonadIO (..), liftIO)
import           Control.Monad.Trans.Reader (ReaderT (..), ask, runReaderT)
import qualified Data.Map                   as M (fromList)
import           Data.Text                  (Text)
import           Database.Bolt.NeoValue
import           Database.Bolt.Protocol

type BoltActionT = ReaderT Pipe

run :: MonadIO m => Pipe -> BoltActionT m a -> m a
run pipe action = runReaderT action pipe

query_ :: MonadIO m => Text -> BoltActionT m ()
query_ cypher = do pipe <- ask
                   let request = RequestRun cypher (M.fromList [])
                   flush pipe request
                   response <- fetch pipe
                   when (isFailure response) $ do
                     processFailure pipe
                     fail ""
                   return ()

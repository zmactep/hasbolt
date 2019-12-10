{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec
import           Test.QuickCheck

import           Data.Text                      ( Text, pack )
import           Data.Default                   ( def  )
import           Data.Monoid                    ( (<>) )

import           Control.Monad                  ( forM_ )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Except           ( MonadError (..) )

import           Database.Bolt

main :: IO ()
main = hspec transactionTests

transactionTests :: Spec
transactionTests = describe "Transaction" $ do
    it "commits correct set of queries" $ do
        pipe <- mkPipe
        name <- run pipe (correct [1, 2])
        name `shouldBe` ("node-X" :: Text)
        closePipe pipe
    it "rollbacks on malformed queries" $ do
        pipe <- mkPipe
        let idx = 3
        run pipe (malformed idx) `shouldThrow` anyException
        checkRollback pipe idx 0
        closePipe pipe
    it "rollbacks on wrong query postprocessing" $ do
        pipe <- mkPipe
        let idx = 5
        run pipe (wrongReturnType idx) `shouldThrow` anyException
        checkRollback pipe idx 0
        closePipe pipe
    it "does not rollback if exception is caught" $ do
        pipe <- mkPipe
        let idx = 6
        number <- run pipe (wrongReturnTypeCaught idx)
        number `shouldBe` -1
        checkRollback pipe idx 1
        closePipe pipe

-- Helper functions

mkPipe :: IO Pipe
mkPipe = connect $ def { user = "neo4j", password = "12345" } 

checkRollback :: Pipe -> Int -> Int -> Expectation
checkRollback pipe idx len = do
    checkResult <- run pipe $ matchQuery idx
    length checkResult `shouldBe` len

closePipe :: Pipe -> IO ()
closePipe pipe = do
    run pipe $ query "MATCH (n:TransactionTestNode) DELETE n"
    close pipe

correct :: [Int] -> BoltActionT IO Text
correct idxs = transact $ do
    forM_ idxs createQuery
    createQueryPostpro

malformed :: Int -> BoltActionT IO Text
malformed i = transact $ do 
    createQuery i
    querySyntaxError
    createQueryPostpro

wrongReturnType :: Int -> BoltActionT IO Int
wrongReturnType idx = transact $ do 
    createQuery idx
    createQueryPostpro

wrongReturnTypeCaught :: Int -> BoltActionT IO Int
wrongReturnTypeCaught idx = transact $ do 
    createQuery idx
    createQueryPostpro `catchError` \e@(WrongMessageFormat _) -> liftIO (print e) >> pure (-1)

createQuery :: Int -> BoltActionT IO [Record]
createQuery i = query $ "CREATE (n:TransactionTestNode {name: \"node-" <> pack (show i) <> "\"}) RETURN n"

createQueryPostpro :: RecordValue a => BoltActionT IO a
createQueryPostpro = do records <- query "CREATE (n:TransactionTestNode {name: \"node-X\"}) RETURN n.name"
                        head records `at` "n.name"

querySyntaxError :: BoltActionT IO [Record]
querySyntaxError = query "Oops! Wrong query here."

matchQuery :: Int -> BoltActionT IO [Record]
matchQuery i = query $ "MATCH (n:TransactionTestNode) WHERE n.name=\"node-" <> pack (show i) <> "\" RETURN n.name"


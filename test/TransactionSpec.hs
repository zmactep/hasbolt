{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec
import           Test.QuickCheck

import           Data.Text                      ( Text, pack )
import           Data.Default                   ( def  )
import           Data.Monoid                    ( (<>) )

import           Database.Bolt

main :: IO ()
main = hspec transactionTests

transactionTests :: Spec
transactionTests = describe "Transaction" $ do
    it "commits correct set of queries" $ do
        pipe <- mkPipe
        name <- run pipe correct
        name `shouldBe` ("node-X" :: Text)
        closePipe pipe
    it "rollback on malformed queries" $ do
        pipe <- mkPipe
        let idx = 3
        run pipe (malformed idx) `shouldThrow` anyException
        checkRollback pipe idx
        closePipe pipe
    it "rollback on wrong query postprocessing" $ do
        pipe <- mkPipe
        let idx = 4
        run pipe (postpro idx) `shouldThrow` anyException
        checkRollback pipe idx
        closePipe pipe

-- Helper functions

mkPipe :: IO Pipe
mkPipe = connect $ def { user = "neo4j", password = "test" } 

checkRollback :: Pipe -> Int -> Expectation
checkRollback pipe idx = do
    checkResult <- run pipe $ matchQuery idx
    checkResult `shouldBe` []

closePipe :: Pipe -> IO ()
closePipe pipe = do
    run pipe $ query "MATCH (n:TransactionTestNode) DELETE n"
    close pipe

correct :: BoltActionT IO Text
correct = transact $ do
    createQuery 1
    createQuery 2
    createQueryPostpro

malformed :: Int -> BoltActionT IO Text
malformed i = transact $ do 
    createQuery i
    querySyntaxError
    createQueryPostpro

postpro :: Int -> BoltActionT IO Int
postpro idx = transact $ do 
    createQuery idx
    createQueryPostpro

createQuery :: Int -> BoltActionT IO [Record]
createQuery i = query $ "CREATE (n:TransactionTestNode {name: \"node-" <> pack (show i) <> "\"}) RETURN n"

createQueryPostpro :: RecordValue a => BoltActionT IO a
createQueryPostpro = do records <- query "CREATE (n:TransactionTestNode {name: \"node-X\"}) RETURN n.name"
                        name <- head records `at` "n.name"
                        exact name

querySyntaxError :: BoltActionT IO [Record]
querySyntaxError = query "Oops! Wrong query here."

matchQuery :: Int -> BoltActionT IO [Record]
matchQuery i = query $ "MATCH (n:TransactionTestNode) WHERE n.name=\"node-" <> pack (show i) <> "\" RETURN n.name"


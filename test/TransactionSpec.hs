{-# LANGUAGE OverloadedStrings #-}

module TransactionSpec
  ( transactionTests
  )
where

import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Maybe                     ( maybeToList )
import           Data.Monoid                    ( (<>) )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Test.Hspec

import           Data.Default                   ( Default(..) )
import           Database.Bolt           hiding ( pack )

mkConnection :: IO Pipe
mkConnection = connect $ def { user = "neo4j", password = "test" }

anyTxError :: Selector TxError
anyTxError = const True

transactionTests :: Spec
transactionTests = describe "Transaction" $ do
  it "commits" $ do
    pipe    <- mkConnection
    persons <- toEntityList "p" <$> run pipe (transact cyphers) :: IO [Person]
    close pipe
    persons `shouldBe` input
  it "rollsback due to malformed cypher query" $ do
    pipe    <- mkConnection
    let stmt = toEntityList "g" <$> run pipe (transact failedCyphers) :: IO [Guitar]
    stmt `shouldThrow` anyTxError
    close pipe

-- Test input data

input :: [Person]
input = [Person "Simon" 61, Person "Philip" 63, Person "Erik" 56]

cyphers :: [Cypher]
cyphers = toCypher <$> input where
  toCypher p = Cypher
    (  "CREATE (p:Person { name: '"
    <> name p
    <> "', age: "
    <> pack (show $ age p)
    <> " } ) RETURN p"
    )
    M.empty

failedCyphers :: [Cypher]
failedCyphers =
  [ Cypher "CREATE (g:Guitar { brand: 'Gibson' } ) RETURN g"   M.empty
  , Cypher "CREATE (g:Guitar { brand: 'Fender' } ) RETURN g"   M.empty
  , Cypher "CREATE (g:Guitar { brand: 'Ibanez' } ) RETURN g"   M.empty
  , Cypher "This is going to make it fail, BOOM!"              M.empty
  , Cypher "CREATE (g:Guitar { brand: 'Schecter' } ) RETURN g" M.empty
  ]

-- Record parser helpers

type NodeProps = Map Text Value

class NodeMapper a where
  toEntity :: NodeProps -> Maybe a

toNode :: Monad m => Text -> Record -> m Node
toNode identifier record = record `at` identifier >>= exact

toNodeProps :: Monad m => Text -> Record -> m NodeProps
toNodeProps identifier r = nodeProps <$> toNode identifier r

toEntityList :: NodeMapper a => Text -> [Record] -> [a]
toEntityList identifier records = records >>= maybeToList . f
  where f r = (toNodeProps identifier r :: Maybe NodeProps) >>= toEntity

newtype Guitar = Guitar { brand :: Text } deriving (Eq, Show)

data Person = Person
  { name :: Text
  , age :: Int
  } deriving (Eq, Show)

instance NodeMapper Guitar where
  toEntity p = Guitar <$> (M.lookup "brand" p >>= exact :: Maybe Text)

instance NodeMapper Person where
  toEntity p =
    Person
      <$> (M.lookup "name" p >>= exact :: Maybe Text)
      <*> (M.lookup "age" p >>= exact :: Maybe Int)

{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative  ((<$>))
import           Data.Binary.Put      (runPut)
import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as BS
import           Data.ByteString.Lazy (fromStrict, toStrict)
import           Data.Either          (isLeft)
import           Data.Map             (Map)
import qualified Data.Map             as M (empty, fromList, lookup)
import           Data.Text            (Text)
import qualified Data.Text            as T (pack, unpack)
import           Hex
import           Test.Hspec
#if !MIN_VERSION_base(4, 13, 0)
import           Control.Monad.Fail (MonadFail)
#endif

import           Database.Bolt
import           Database.Bolt.Connection.Instances  (dbExtra, helloMap, notifExtra)
import           Database.Bolt.Connection.RouterPool (IdlePipe (..), PoolState (..), ServerPool (..),
                                                      isConnectionError, reconcileState)
import           Database.Bolt.Connection.Type       (AuthToken (..), Request (..), Response (..),
                                                      ResponseError (..))
import           Database.Bolt.Serialization
import           Database.Bolt.Value.Helpers         (isV3, isV4_3, isV5, isV5_2, isV5_3, isV5_6)

import           Data.Bits          (shiftR, (.&.))
import           Data.Default       (def)
import qualified Data.Map.Strict    as MS
import           Data.Time.Calendar (fromGregorian)
import           Data.Time.Clock    (NominalDiffTime, UTCTime (..), addUTCTime, secondsToDiffTime)
import           Data.Word          (Word32)

main :: IO ()
main = hspec $ do
         packStreamTests
         unpackStreamTests
         v5Tests
         v58Tests
         routingTests
         routerTests
         helloMapTests
         notifExtraTests
         toStructureRequestTests
         isConnectionErrorTests
         reconcileStateTests
         largerBytesTests
         parseAddressEdgeCaseTests

pack' :: BoltValue a => a -> ByteString
pack' = toStrict . runPut . pack

unpackStreamTests :: Spec
unpackStreamTests =
  describe "Unpack" $ do
    it "unpacks integers correct" $ do
      u1 <- prepareData "01" >>= unpackF :: IO Int
      u1 `shouldBe` 1
      u42 <- prepareData "2A" >>= unpackF :: IO Int
      u42 `shouldBe` 42
      u1234 <- prepareData "C904D2" >>= unpackF :: IO Int
      u1234 `shouldBe` 1234
    it "unpacks doubles correct" $ do
      u6d <- prepareData "C1401921FB54442D18" >>= unpackF :: IO Double
      u6d `shouldBe` 6.283185307179586
      um1d <- prepareData "C1BFF199999999999A" >>= unpackF :: IO Double
      um1d `shouldBe` (-1.1)
    it "unpacks booleans correct" $ do
      uF <- prepareData "C2" >>= unpackF :: IO Bool
      uF `shouldBe` False
      uT <- prepareData "C3" >>= unpackF :: IO Bool
      uT `shouldBe` True
    it "unpacks strings correct" $ do
      usE <- prepareData "80" >>= unpackF :: IO Text
      usE `shouldBe` T.pack ""
      usA <- prepareData "8141" >>= unpackF :: IO Text
      usA `shouldBe` T.pack "A"
      usU <- prepareData "D0124772C3B6C39F656E6D61C39F7374C3A46265" >>= unpackF :: IO Text
      usU `shouldBe` T.pack "Größenmaßstäbe"
    it "unpacks lists correct" $ do
      ulE <- prepareData "90" >>= unpackF :: IO [Int]
      ulE `shouldBe` []
      ulI <- prepareData "93010203" >>= unpackF :: IO [Int]
      ulI `shouldBe` [1,2,3]
      ulL <- prepareData "D4280102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F202122232425262728" >>= unpackF :: IO [Int]
      ulL `shouldBe` [1..40]
    it "unpacks dicts correct" $ do
      udE <- prepareData "A0" >>= unpackF :: IO (Map Text ())
      udE `shouldBe` M.fromList []
      udS <- prepareData "A1836F6E658465696E73" >>= unpackF :: IO (Map Text Text)
      udS `shouldBe` M.fromList [(T.pack "one", T.pack "eins")]
    it "unpacks () correct" $ do
      uN <- prepareData "C0" >>= unpackF :: IO ()
      uN `shouldBe` ()

packStreamTests :: Spec
packStreamTests =
  describe "Pack" $ do
    it "packs integers correct" $ do
      hex (pack' (1::Int)) `shouldBe` "01"
      hex (pack' (42::Int)) `shouldBe` "2A"
      hex (pack' (1234::Int)) `shouldBe` "C904D2"
    it "packs doubles correct" $ do
      hex (pack' (6.283185307179586::Double)) `shouldBe` "C1401921FB54442D18"
      hex (pack' (-1.1::Double)) `shouldBe` "C1BFF199999999999A"
    it "packs booleans correct" $ do
      hex (pack' False) `shouldBe` "C2"
      hex (pack' True) `shouldBe` "C3"
    it "packs strings correct" $ do
      hex (pack' $ T.pack "") `shouldBe` "80"
      hex (pack' $ T.pack "A") `shouldBe` "8141"
      hex (pack' $ T.pack "Größenmaßstäbe") `shouldBe` "D0124772C3B6C39F656E6D61C39F7374C3A46265"
      hex (pack' $ T.pack "ABCDEFGHIJKLMNOPQRSTUVWXYZ") `shouldBe` "D01A4142434445464748494A4B4C4D4E4F505152535455565758595A"
    it "packs lists correct" $ do
      hex (pack' ([]::[Int])) `shouldBe` "90"
      hex (pack' ([1,2,3]::[Int])) `shouldBe` "93010203"
      hex (pack' ([1..40]::[Int])) `shouldBe` "D4280102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F202122232425262728"
    it "packs dicts correct" $ do
      hex (pack' (M.empty :: Map Text ())) `shouldBe` "A0"
      hex (pack' (M.fromList [(T.pack "one", T.pack "eins")])) `shouldBe` "A1836F6E658465696E73"
    it "packs () correct" $
      hex (pack' ()) `shouldBe` "C0"

prepareData :: MonadFail m => ByteString -> m ByteString
prepareData = (toStrict <$>) . unhex . fromStrict

-- | Helper to unpack a Structure from a ByteString
unpackStruct :: ByteString -> Either UnpackError Structure
unpackStruct bs = case unpackAction unpackT (fromStrict bs) of
                    Left e  -> Left e
                    Right s -> Right s

v5Tests :: Spec
v5Tests =
  describe "Bolt v5" $ do
    it "unpacks Node with 4 fields (v5 element_id)" $ do
      -- Structure with sig=0x4E (78), 4 fields: I 42, L ["Person"], M {}, T "4:xxx:42"
      let nodeStruct = Structure 78 [I 42, L [T (T.pack "Person")], M M.empty, T (T.pack "4:xxx:42")]
          bs = pack' nodeStruct
          result = unpackStruct bs >>= fromStructure :: Either UnpackError Node
      case result of
        Right n -> do
          nodeIdentity n `shouldBe` 42
          labels n `shouldBe` [T.pack "Person"]
          nodeProps n `shouldBe` M.empty
          nodeElementId n `shouldBe` T.pack "4:xxx:42"
        Left e -> expectationFailure (show e)

    it "unpacks Node with 3 fields (v3 compat)" $ do
      let nodeStruct = Structure 78 [I 1, L [T (T.pack "A")], M M.empty]
          bs = pack' nodeStruct
          result = unpackStruct bs >>= fromStructure :: Either UnpackError Node
      case result of
        Right n -> do
          nodeIdentity n `shouldBe` 1
          nodeElementId n `shouldBe` T.pack ""
        Left e -> expectationFailure (show e)

    it "unpacks Relationship with 8 fields (v5)" $ do
      let relStruct = Structure 82 [ I 10, I 1, I 2
                                    , T (T.pack "KNOWS"), M M.empty
                                    , T (T.pack "5:xxx:10")
                                    , T (T.pack "5:xxx:1")
                                    , T (T.pack "5:xxx:2")
                                    ]
          bs = pack' relStruct
          result = unpackStruct bs >>= fromStructure :: Either UnpackError Relationship
      case result of
        Right r -> do
          relIdentity r `shouldBe` 10
          startNodeId r `shouldBe` 1
          endNodeId r `shouldBe` 2
          relType r `shouldBe` T.pack "KNOWS"
          relElementId r `shouldBe` T.pack "5:xxx:10"
          startNodeElementId r `shouldBe` T.pack "5:xxx:1"
          endNodeElementId r `shouldBe` T.pack "5:xxx:2"
        Left e -> expectationFailure (show e)

    it "unpacks Relationship with 5 fields (v3 compat)" $ do
      let relStruct = Structure 82 [I 10, I 1, I 2, T (T.pack "KNOWS"), M M.empty]
          bs = pack' relStruct
          result = unpackStruct bs >>= fromStructure :: Either UnpackError Relationship
      case result of
        Right r -> do
          relIdentity r `shouldBe` 10
          relElementId r `shouldBe` T.pack ""
        Left e -> expectationFailure (show e)

    it "unpacks URelationship with 4 fields (v5)" $ do
      let urelStruct = Structure 114 [I 7, T (T.pack "LIKES"), M M.empty, T (T.pack "5:xxx:7")]
          bs = pack' urelStruct
          result = unpackStruct bs >>= fromStructure :: Either UnpackError URelationship
      case result of
        Right r -> do
          urelIdentity r `shouldBe` 7
          urelElementId r `shouldBe` T.pack "5:xxx:7"
        Left e -> expectationFailure (show e)

    it "unpacks URelationship with 3 fields (v3 compat)" $ do
      let urelStruct = Structure 114 [I 7, T (T.pack "LIKES"), M M.empty]
          bs = pack' urelStruct
          result = unpackStruct bs >>= fromStructure :: Either UnpackError URelationship
      case result of
        Right r -> do
          urelIdentity r `shouldBe` 7
          urelElementId r `shouldBe` T.pack ""
        Left e -> expectationFailure (show e)

    it "packs LOGON request as structure" $ do
      -- sigLogon = 0x6A, one field: dict with auth info
      let logonStruct = Structure 0x6A [M (M.fromList [ (T.pack "scheme", T (T.pack "basic"))
                                                       , (T.pack "principal", T (T.pack "neo4j"))
                                                       , (T.pack "credentials", T (T.pack "pass"))
                                                       ])]
          bs = pack' logonStruct
          result = unpackStruct bs
      case result of
        Right (Structure sig [M _m]) -> sig `shouldBe` 0x6A
        Right _                      -> expectationFailure "unexpected structure shape"
        Left e                       -> expectationFailure (show e)

    it "packs LOGOFF request as structure" $ do
      let logoffStruct = Structure 0x6B []
          bs = pack' logoffStruct
          result = unpackStruct bs
      case result of
        Right (Structure sig []) -> sig `shouldBe` 0x6B
        Right _                  -> expectationFailure "unexpected structure shape"
        Left e                   -> expectationFailure (show e)

    it "packs PULL with extra dict as structure" $ do
      let pullStruct = Structure 0x3F [M (M.fromList [(T.pack "n", I (-1))])]
          bs = pack' pullStruct
          result = unpackStruct bs
      case result of
        Right (Structure sig [M m]) -> do
          sig `shouldBe` 0x3F
          M.lookup (T.pack "n") m `shouldBe` Just (I (-1))
        Right _  -> expectationFailure "unexpected structure shape"
        Left e   -> expectationFailure (show e)

    it "packs DISCARD with extra dict as structure" $ do
      let discardStruct = Structure 0x2F [M (M.fromList [(T.pack "n", I (-1))])]
          bs = pack' discardStruct
          result = unpackStruct bs
      case result of
        Right (Structure sig [M m]) -> do
          sig `shouldBe` 0x2F
          M.lookup (T.pack "n") m `shouldBe` Just (I (-1))
        Right _  -> expectationFailure "unexpected structure shape"
        Left e   -> expectationFailure (show e)

v58Tests :: Spec
v58Tests =
  describe "Bolt 5.2-5.8" $ do
    describe "version checks (real library functions)" $ do
      it "isV3 returns True for v3" $
        isV3 3 `shouldBe` True
      it "isV3 returns False for v2" $
        isV3 2 `shouldBe` False
      it "isV3 returns True for v5.1" $
        isV3 0x0105 `shouldBe` True
      it "isV5 returns True for v5.1" $
        isV5 0x0105 `shouldBe` True
      it "isV5 returns False for v3" $
        isV5 3 `shouldBe` False
      it "isV5 returns False for v4.3" $
        isV5 0x0304 `shouldBe` False
      it "isV4_3 returns True for v4.3" $
        isV4_3 0x0304 `shouldBe` True
      it "isV4_3 returns True for v5.0" $
        isV4_3 0x0005 `shouldBe` True
      it "isV4_3 returns False for v4.2" $
        isV4_3 0x0204 `shouldBe` False
      it "isV5_2 returns False for v5.1" $
        isV5_2 0x0105 `shouldBe` False
      it "isV5_2 returns True for v5.2" $
        isV5_2 0x0205 `shouldBe` True
      it "isV5_3 returns False for v5.1" $
        isV5_3 0x0105 `shouldBe` False
      it "isV5_3 returns True for v5.3" $
        isV5_3 0x0305 `shouldBe` True
      it "isV5_3 returns True for v5.8" $
        isV5_3 0x0805 `shouldBe` True
      it "isV5_6 returns False for v5.3" $
        isV5_6 0x0305 `shouldBe` False
      it "isV5_6 returns True for v5.6" $
        isV5_6 0x0605 `shouldBe` True
      it "isV5 returns True for major > 5" $
        isV5 6 `shouldBe` True

    describe "large size roundtrips (unsigned size bytes)" $ do
      it "roundtrips text >= 128 bytes (TEXT_8 with size >= 0x80)" $ do
        let longText = T.pack (replicate 200 'x')
            packed = pack' longText
        result <- unpackF packed :: IO Text
        result `shouldBe` longText

      it "roundtrips list >= 128 elements (LIST_8 with size >= 0x80)" $ do
        let longList = [1..200] :: [Int]
            packed = pack' longList
        result <- unpackF packed :: IO [Int]
        result `shouldBe` longList

    describe "TELEMETRY message (sig 0x54)" $ do
      it "packs and unpacks TELEMETRY structure" $ do
        let telStruct = Structure 0x54 [I 7]
            bs = pack' telStruct
            result = unpackStruct bs
        case result of
          Right (Structure sig [I n]) -> do
            sig `shouldBe` 0x54
            n `shouldBe` 7
          Right _  -> expectationFailure "unexpected structure shape"
          Left e   -> expectationFailure (show e)

    describe "Bytes value support" $ do
      it "packs and unpacks Bytes value" $ do
        let bsVal = Bytes (BS.pack [0x01, 0x02, 0x03])
            packed = pack' bsVal
            result = unpackAction unpackT (fromStrict packed) :: Either UnpackError Value
        result `shouldBe` Right bsVal

      it "roundtrips empty Bytes" $ do
        let bsVal = Bytes BS.empty
            packed = pack' bsVal
            result = unpackAction unpackT (fromStrict packed) :: Either UnpackError Value
        result `shouldBe` Right bsVal

    describe "BoltCfg defaults" $ do
      it "default version is 5.8 with range" $ do
        let cfg = def :: BoltCfg
        version cfg `shouldBe` (0x00070805 :: Word32)
      it "default userAgent is hasbolt/1.8" $ do
        let cfg = def :: BoltCfg
        userAgent cfg `shouldBe` T.pack "hasbolt/1.8"
      it "default notifMinSeverity is Nothing" $ do
        let cfg = def :: BoltCfg
        notifMinSeverity cfg `shouldBe` Nothing
      it "default notifDisabledClass is empty" $ do
        let cfg = def :: BoltCfg
        notifDisabledClass cfg `shouldBe` []
      it "default database is Nothing" $ do
        let cfg = def :: BoltCfg
        database cfg `shouldBe` Nothing

routingTests :: Spec
routingTests =
  describe "Routing primitives" $ do
    describe "ROUTE message (sig 0x66)" $ do
      it "packs and unpacks ROUTE with 3 fields" $ do
        let ctx = M.fromList [(T.pack "address", T (T.pack "localhost:7687"))]
            bmarks = L [T (T.pack "bm1"), T (T.pack "bm2")]
            extra = M.fromList [(T.pack "db", T (T.pack "neo4j"))]
            routeStruct = Structure 0x66 [M ctx, bmarks, M extra]
            bs = pack' routeStruct
            result = unpackStruct bs
        case result of
          Right (Structure sig [M ctx', L bmarks', M extra']) -> do
            sig `shouldBe` 0x66
            M.lookup (T.pack "address") ctx' `shouldBe` Just (T (T.pack "localhost:7687"))
            bmarks' `shouldBe` [T (T.pack "bm1"), T (T.pack "bm2")]
            M.lookup (T.pack "db") extra' `shouldBe` Just (T (T.pack "neo4j"))
          Right _ -> expectationFailure "unexpected structure shape"
          Left e  -> expectationFailure (show e)

      it "packs ROUTE with empty context and bookmarks" $ do
        let routeStruct = Structure 0x66 [M M.empty, L [], M M.empty]
            bs = pack' routeStruct
            result = unpackStruct bs
        case result of
          Right (Structure sig [M ctx, L bmarks, M extra]) -> do
            sig `shouldBe` 0x66
            ctx `shouldBe` M.empty
            bmarks `shouldBe` []
            extra `shouldBe` M.empty
          Right _ -> expectationFailure "unexpected structure shape"
          Left e  -> expectationFailure (show e)

    describe "HELLO with routing context" $ do
      it "includes routing key in HELLO dict" $ do
        -- Simulate a HELLO dict that includes a routing key
        let routingCtx = M.fromList [(T.pack "address", T (T.pack "localhost:7687"))]
            helloDict = M.fromList [ (T.pack "user_agent", T (T.pack "hasbolt/1.8"))
                                   , (T.pack "routing", M routingCtx)
                                   ]
            helloStruct = Structure 0x01 [M helloDict]
            bs = pack' helloStruct
            result = unpackStruct bs
        case result of
          Right (Structure sig [M m]) -> do
            sig `shouldBe` 0x01
            case M.lookup (T.pack "routing") m of
              Just (M r) -> M.lookup (T.pack "address") r `shouldBe` Just (T (T.pack "localhost:7687"))
              _          -> expectationFailure "routing key missing or wrong type"
          Right _ -> expectationFailure "unexpected structure shape"
          Left e  -> expectationFailure (show e)

    describe "BEGIN with db and mode" $ do
      it "serializes BEGIN with db and mode keys" $ do
        let extra = M.fromList [(T.pack "db", T (T.pack "mydb")), (T.pack "mode", T (T.pack "r"))]
            beginStruct = Structure 0x11 [M extra]
            bs = pack' beginStruct
            result = unpackStruct bs
        case result of
          Right (Structure sig [M m]) -> do
            sig `shouldBe` 0x11
            M.lookup (T.pack "db") m `shouldBe` Just (T (T.pack "mydb"))
            M.lookup (T.pack "mode") m `shouldBe` Just (T (T.pack "r"))
          Right _ -> expectationFailure "unexpected structure shape"
          Left e  -> expectationFailure (show e)

-- | A fixed time for testing.
testTime :: UTCTime
testTime = UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 0)

routerTests :: Spec
routerTests =
  describe "Router" $ do
    describe "parseAddress" $ do
      it "parses localhost:7687" $
        parseAddress (T.pack "localhost:7687") `shouldBe` Right (ServerAddress "localhost" 7687)

      it "parses IP address:port" $
        parseAddress (T.pack "192.168.1.1:7474") `shouldBe` Right (ServerAddress "192.168.1.1" 7474)

      it "parses IPv6 address [::1]:7687" $
        parseAddress (T.pack "[::1]:7687") `shouldBe` Right (ServerAddress "::1" 7687)

      it "rejects address without port" $
        case parseAddress (T.pack "badaddress") of
          Left _  -> pure ()
          Right _ -> expectationFailure "expected Left for invalid address"

      it "rejects empty address" $
        case parseAddress (T.pack "") of
          Left _  -> pure ()
          Right _ -> expectationFailure "expected Left for empty address"

    describe "parseRoutingTable" $ do
      it "parses a valid routing table" $ do
        let rtMap = M.fromList
              [ (T.pack "ttl", I 300)
              , (T.pack "servers", L
                  [ M (M.fromList [(T.pack "role", T (T.pack "WRITE")), (T.pack "addresses", L [T (T.pack "w1:7687")])])
                  , M (M.fromList [(T.pack "role", T (T.pack "READ")),  (T.pack "addresses", L [T (T.pack "r1:7687"), T (T.pack "r2:7687")])])
                  , M (M.fromList [(T.pack "role", T (T.pack "ROUTE")), (T.pack "addresses", L [T (T.pack "rt1:7687")])])
                  ])
              ]
        case parseRoutingTable testTime rtMap of
          Right rt -> do
            length (rtReaders rt) `shouldBe` 2
            length (rtWriters rt) `shouldBe` 1
            length (rtRouters rt) `shouldBe` 1
            rtTTL rt `shouldBe` 300
          Left err -> expectationFailure (T.unpack err)

      it "rejects missing servers" $ do
        let rtMap = M.fromList [(T.pack "ttl", I 300)]
        case parseRoutingTable testTime rtMap of
          Left _  -> pure ()
          Right _ -> expectationFailure "expected Left for missing servers"

      it "rejects empty writers" $ do
        let rtMap = M.fromList
              [ (T.pack "ttl", I 300)
              , (T.pack "servers", L
                  [ M (M.fromList [(T.pack "role", T (T.pack "READ")), (T.pack "addresses", L [T (T.pack "r1:7687")])])
                  , M (M.fromList [(T.pack "role", T (T.pack "ROUTE")), (T.pack "addresses", L [T (T.pack "rt1:7687")])])
                  ])
              ]
        case parseRoutingTable testTime rtMap of
          Left _  -> pure ()
          Right _ -> expectationFailure "expected Left for empty writers"

      it "parses routing table nested under rt key" $ do
        let inner = M.fromList
              [ (T.pack "ttl", I 600)
              , (T.pack "servers", L
                  [ M (M.fromList [(T.pack "role", T (T.pack "WRITE")), (T.pack "addresses", L [T (T.pack "w1:7687")])])
                  , M (M.fromList [(T.pack "role", T (T.pack "READ")),  (T.pack "addresses", L [T (T.pack "r1:7687")])])
                  , M (M.fromList [(T.pack "role", T (T.pack "ROUTE")), (T.pack "addresses", L [T (T.pack "rt1:7687")])])
                  ])
              ]
            rtMap = M.fromList [(T.pack "rt", M inner)]
        case parseRoutingTable testTime rtMap of
          Right rt -> do
            rtTTL rt `shouldBe` 600
            length (rtWriters rt) `shouldBe` 1
          Left err -> expectationFailure (T.unpack err)

    describe "isExpired" $ do
      it "returns False when expiry is in the future" $ do
        let rt = RoutingTable [] [] [] 300
                   (addUTCTime (600 :: NominalDiffTime) testTime)
        isExpired testTime rt `shouldBe` False

      it "returns True when expiry is in the past" $ do
        let rt = RoutingTable [] [] [] 300
                   (addUTCTime (-1 :: NominalDiffTime) testTime)
        isExpired testTime rt `shouldBe` True

      it "returns True when now == expiry (boundary)" $ do
        let rt = RoutingTable [] [] [] 300 testTime
        isExpired testTime rt `shouldBe` True

-- | Tests for helloMap (issue #1: zero coverage)
helloMapTests :: Spec
helloMapTests =
  describe "helloMap" $ do
    let auth = AuthToken "basic" "neo4j" "secret"
        routingCtx = M.fromList [("address", T "localhost:7687")]

    it "v3: includes user_agent and credentials inline" $ do
      let m = helloMap "hasbolt/1.8" auth 3 Nothing
      M.lookup "user_agent" m `shouldBe` Just (T "hasbolt/1.8")
      M.lookup "scheme" m `shouldBe` Just (T "basic")
      M.lookup "principal" m `shouldBe` Just (T "neo4j")
      M.lookup "credentials" m `shouldBe` Just (T "secret")
      -- v3 should not have routing or bolt_agent
      M.lookup "routing" m `shouldBe` Nothing
      M.lookup "bolt_agent" m `shouldBe` Nothing

    it "v5.1: includes user_agent, omits credentials" $ do
      let m = helloMap "hasbolt/1.8" auth 0x0105 Nothing
      M.lookup "user_agent" m `shouldBe` Just (T "hasbolt/1.8")
      M.lookup "scheme" m `shouldBe` Nothing
      M.lookup "credentials" m `shouldBe` Nothing
      M.lookup "bolt_agent" m `shouldBe` Nothing

    it "v5.1 with routing context" $ do
      let m = helloMap "hasbolt/1.8" auth 0x0105 (Just routingCtx)
      M.lookup "routing" m `shouldBe` Just (M routingCtx)
      M.lookup "credentials" m `shouldBe` Nothing

    it "v5.3: includes bolt_agent" $ do
      let m = helloMap "hasbolt/1.8" auth 0x0305 Nothing
      case M.lookup "bolt_agent" m of
        Just (M agent) -> M.lookup "product" agent `shouldBe` Just (T "hasbolt/1.8")
        _              -> expectationFailure "bolt_agent missing or wrong type"
      M.lookup "credentials" m `shouldBe` Nothing

    it "v5.3 with routing context includes both bolt_agent and routing" $ do
      let m = helloMap "hasbolt/1.8" auth 0x0305 (Just routingCtx)
      M.lookup "bolt_agent" m `shouldSatisfy` (/= Nothing)
      M.lookup "routing" m `shouldBe` Just (M routingCtx)

    it "v3: routing context is ignored" $ do
      let m = helloMap "hasbolt/1.8" auth 3 (Just routingCtx)
      -- v3 path ignores mRouting, just inlines creds
      M.lookup "routing" m `shouldBe` Nothing
      M.lookup "credentials" m `shouldBe` Just (T "secret")

-- | Tests for notifExtra (issue #2: zero coverage)
notifExtraTests :: Spec
notifExtraTests =
  describe "notifExtra" $ do
    it "returns empty for pre-v5.2" $ do
      notifExtra 0x0105 (Just "WARNING") ["HINT"] `shouldBe` M.empty
      notifExtra 3 (Just "OFF") [] `shouldBe` M.empty

    it "v5.2: includes severity when present" $ do
      let m = notifExtra 0x0205 (Just "WARNING") []
      M.lookup "notifications_minimum_severity" m `shouldBe` Just (T "WARNING")

    it "v5.2: omits severity when Nothing" $ do
      let m = notifExtra 0x0205 Nothing []
      M.lookup "notifications_minimum_severity" m `shouldBe` Nothing

    it "v5.2: uses notifications_disabled_categories key" $ do
      let m = notifExtra 0x0205 Nothing ["HINT", "DEPRECATION"]
      case M.lookup "notifications_disabled_categories" m of
        Just (L cats) -> cats `shouldBe` [T "HINT", T "DEPRECATION"]
        _             -> expectationFailure "disabled categories missing"

    it "v5.6: uses notifications_disabled_classifications key" $ do
      let m = notifExtra 0x0605 Nothing ["HINT"]
      M.lookup "notifications_disabled_classifications" m `shouldBe` Just (L [T "HINT"])
      -- old key should not be present
      M.lookup "notifications_disabled_categories" m `shouldBe` Nothing

    it "v5.2: includes both severity and disabled" $ do
      let m = notifExtra 0x0205 (Just "OFF") ["HINT"]
      M.lookup "notifications_minimum_severity" m `shouldBe` Just (T "OFF")
      M.lookup "notifications_disabled_categories" m `shouldBe` Just (L [T "HINT"])

    it "v5.2: empty disabled list produces no key" $ do
      let m = notifExtra 0x0205 Nothing []
      M.lookup "notifications_disabled_categories" m `shouldBe` Nothing

-- | Tests for ToStructure Request instances (issue #4)
toStructureRequestTests :: Spec
toStructureRequestTests =
  describe "ToStructure Request" $ do
    let auth = AuthToken "basic" "neo4j" "pass"
        routingCtx = M.fromList [("address", T "localhost:7687")]
        roundtrip req = unpackStruct (pack' (toStructure req))

    it "RequestLogon produces sig 0x6A with token map" $ do
      case roundtrip (RequestLogon auth) of
        Right (Structure sig [M m]) -> do
          sig `shouldBe` 0x6A
          M.lookup "scheme" m `shouldBe` Just (T "basic")
          M.lookup "principal" m `shouldBe` Just (T "neo4j")
          M.lookup "credentials" m `shouldBe` Just (T "pass")
        other -> expectationFailure ("unexpected: " <> show other)

    it "RequestLogoff produces sig 0x6B with no fields" $ do
      case roundtrip RequestLogoff of
        Right (Structure sig []) -> sig `shouldBe` 0x6B
        other -> expectationFailure ("unexpected: " <> show other)

    it "RequestTelemetry produces sig 0x54 with api int" $ do
      case roundtrip (RequestTelemetry 7) of
        Right (Structure sig [I n]) -> do
          sig `shouldBe` 0x54
          n `shouldBe` 7
        other -> expectationFailure ("unexpected: " <> show other)

    it "RequestPull produces sig 0x3F with extra dict" $ do
      let extra = M.fromList ["n" =: (-1 :: Int)]
      case roundtrip (RequestPull extra) of
        Right (Structure sig [M m]) -> do
          sig `shouldBe` 0x3F
          M.lookup "n" m `shouldBe` Just (I (-1))
        other -> expectationFailure ("unexpected: " <> show other)

    it "RequestDiscard produces sig 0x2F with extra dict" $ do
      let extra = M.fromList ["n" =: (-1 :: Int)]
      case roundtrip (RequestDiscard extra) of
        Right (Structure sig [M m]) -> do
          sig `shouldBe` 0x2F
          M.lookup "n" m `shouldBe` Just (I (-1))
        other -> expectationFailure ("unexpected: " <> show other)

    it "RequestRoute produces sig 0x66 with context, bookmarks, extra" $ do
      let extra = M.fromList ["db" =: ("neo4j" :: Text)]
      case roundtrip (RequestRoute routingCtx ["bm1"] extra) of
        Right (Structure sig [M ctx, L bmarks, M ext]) -> do
          sig `shouldBe` 0x66
          M.lookup "address" ctx `shouldBe` Just (T "localhost:7687")
          bmarks `shouldBe` [T "bm1"]
          M.lookup "db" ext `shouldBe` Just (T "neo4j")
        other -> expectationFailure ("unexpected: " <> show other)

    it "RequestInit v3 produces sig 0x01 with creds inline" $ do
      case roundtrip (RequestInit "hasbolt/1.8" auth 3 Nothing) of
        Right (Structure sig [M m]) -> do
          sig `shouldBe` 0x01
          M.lookup "user_agent" m `shouldBe` Just (T "hasbolt/1.8")
          M.lookup "scheme" m `shouldBe` Just (T "basic")
        other -> expectationFailure ("unexpected: " <> show other)

    it "RequestInit v5 produces sig 0x01 without creds" $ do
      case roundtrip (RequestInit "hasbolt/1.8" auth 0x0105 Nothing) of
        Right (Structure sig [M m]) -> do
          sig `shouldBe` 0x01
          M.lookup "user_agent" m `shouldBe` Just (T "hasbolt/1.8")
          M.lookup "scheme" m `shouldBe` Nothing
        other -> expectationFailure ("unexpected: " <> show other)

    it "RequestInit v5 with routing includes routing key" $ do
      case roundtrip (RequestInit "hasbolt/1.8" auth 0x0105 (Just routingCtx)) of
        Right (Structure sig [M m]) -> do
          sig `shouldBe` 0x01
          M.lookup "routing" m `shouldBe` Just (M routingCtx)
        other -> expectationFailure ("unexpected: " <> show other)

    it "RequestBegin produces sig 0x11 with extra" $ do
      let extra = M.fromList ["mode" =: ("r" :: Text), "db" =: ("mydb" :: Text)]
      case roundtrip (RequestBegin extra) of
        Right (Structure sig [M m]) -> do
          sig `shouldBe` 0x11
          M.lookup "mode" m `shouldBe` Just (T "r")
          M.lookup "db" m `shouldBe` Just (T "mydb")
        other -> expectationFailure ("unexpected: " <> show other)

-- | Tests for isConnectionError (issue #5)
isConnectionErrorTests :: Spec
isConnectionErrorTests =
  describe "isConnectionError" $ do
    it "CannotReadChunk is a connection error" $
      isConnectionError CannotReadChunk `shouldBe` True
    it "ResponseError is not a connection error" $
      isConnectionError (ResponseError UnknownResponseFailure) `shouldBe` False
    it "RoutingTableUnavailable is not a connection error" $
      isConnectionError RoutingTableUnavailable `shouldBe` False
    it "UnsupportedServerVersion is not a connection error" $
      isConnectionError UnsupportedServerVersion `shouldBe` False
    it "AuthentificationFailed is not a connection error" $
      isConnectionError AuthentificationFailed `shouldBe` False

-- | Tests for reconcileState (issue #6)
reconcileStateTests :: Spec
reconcileStateTests =
  describe "reconcileState" $ do
    let mkAddr h p = ServerAddress h p
        addrA = mkAddr "a" 7687
        addrB = mkAddr "b" 7687
        addrC = mkAddr "c" 7687
        emptyPool = ServerPool [] 0

    it "keeps existing servers that remain in new table" $ do
      let pool = ServerPool { spIdle = [], spInUse = 3 }
          ps = PoolState { psServers = MS.fromList [(addrA, pool)], psTable = undefined, psRRIndex = 5 }
          newTable = RoutingTable [addrA] [addrA] [addrA] 300 testTime
          (ps', removed) = reconcileState ps newTable
      MS.member addrA (psServers ps') `shouldBe` True
      -- in-use count preserved
      case MS.lookup addrA (psServers ps') of
        Just sp -> spInUse sp `shouldBe` 3
        Nothing -> expectationFailure "server A missing"
      length removed `shouldBe` 0
      psRRIndex ps' `shouldBe` 5

    it "adds new servers with empty pools" $ do
      let ps = PoolState { psServers = MS.fromList [(addrA, emptyPool)], psTable = undefined, psRRIndex = 0 }
          newTable = RoutingTable [] [addrA, addrB] [] 300 testTime
          (ps', removed) = reconcileState ps newTable
      MS.member addrB (psServers ps') `shouldBe` True
      case MS.lookup addrB (psServers ps') of
        Just sp -> do length (spIdle sp) `shouldBe` 0
                      spInUse sp `shouldBe` 0
        Nothing -> expectationFailure "server B missing"
      length removed `shouldBe` 0

    it "removes servers not in new table, returning their idle pipes" $ do
      let dummyPipe = undefined  -- we won't actually close it, just check the list
          idlePipe = IdlePipe dummyPipe testTime
          pool = ServerPool { spIdle = [idlePipe], spInUse = 0 }
          ps = PoolState { psServers = MS.fromList [(addrA, emptyPool), (addrB, pool)]
                         , psTable = undefined, psRRIndex = 0 }
          newTable = RoutingTable [] [addrA] [] 300 testTime
          (ps', removed) = reconcileState ps newTable
      MS.member addrB (psServers ps') `shouldBe` False
      length removed `shouldBe` 1

    it "handles complete server set replacement" $ do
      let ps = PoolState { psServers = MS.fromList [(addrA, emptyPool), (addrB, emptyPool)]
                         , psTable = undefined, psRRIndex = 0 }
          newTable = RoutingTable [addrC] [addrC] [addrC] 300 testTime
          (ps', removed) = reconcileState ps newTable
      MS.member addrA (psServers ps') `shouldBe` False
      MS.member addrB (psServers ps') `shouldBe` False
      MS.member addrC (psServers ps') `shouldBe` True
      length removed `shouldBe` 0  -- old pools had no idle pipes

-- | Tests for larger Bytes pack/unpack (issue #7)
largerBytesTests :: Spec
largerBytesTests =
  describe "Bytes larger sizes" $ do
    it "roundtrips Bytes with 256 bytes (bytes16 marker 0xCD)" $ do
      let bs = BS.pack (take 256 (cycle [0..255]))
          bsVal = Bytes bs
          packed = pack' bsVal
          result = unpackAction unpackT (fromStrict packed) :: Either UnpackError Value
      result `shouldBe` Right bsVal

    it "roundtrips Bytes with 300 bytes" $ do
      let bs = BS.replicate 300 0xAB
          bsVal = Bytes bs
          packed = pack' bsVal
          result = unpackAction unpackT (fromStrict packed) :: Either UnpackError Value
      result `shouldBe` Right bsVal

    it "roundtrips dict with > 15 entries (dict8 marker)" $ do
      let entries = [(T.pack ("key" <> show i), I i) | i <- [1..20 :: Int]]
          dict = M.fromList entries
          packed = pack' dict
      result <- unpackF packed :: IO (Map Text Value)
      result `shouldBe` dict

    it "roundtrips Bytes with 200 bytes (bytes8 boundary)" $ do
      let bs = BS.replicate 200 0xFF
          bsVal = Bytes bs
          packed = pack' bsVal
          result = unpackAction unpackT (fromStrict packed) :: Either UnpackError Value
      result `shouldBe` Right bsVal

-- | Tests for parseAddress edge cases (issue #8)
parseAddressEdgeCaseTests :: Spec
parseAddressEdgeCaseTests =
  describe "parseAddress edge cases" $ do
    it "rejects port 0" $
      parseAddress "localhost:0" `shouldSatisfy` isLeft

    it "rejects port 65536" $
      parseAddress "localhost:65536" `shouldSatisfy` isLeft

    it "rejects non-numeric port" $
      parseAddress "localhost:abc" `shouldSatisfy` isLeft

    it "rejects trailing text after port" $
      parseAddress "localhost:7687x" `shouldSatisfy` isLeft

    it "parses max valid port 65535" $
      parseAddress "localhost:65535" `shouldBe` Right (ServerAddress "localhost" 65535)

    it "parses port 1 (min valid)" $
      parseAddress "localhost:1" `shouldBe` Right (ServerAddress "localhost" 1)

    it "rejects address with only colon" $
      parseAddress ":" `shouldSatisfy` isLeft

    it "parses hostname with dots" $
      parseAddress "neo4j.example.com:7687" `shouldBe` Right (ServerAddress "neo4j.example.com" 7687)

    it "rejects IPv6 without closing bracket-colon" $
      parseAddress "[::1]7687" `shouldSatisfy` isLeft

    it "parses full IPv6 address" $
      parseAddress "[2001:db8::1]:7687" `shouldBe` Right (ServerAddress "2001:db8::1" 7687)

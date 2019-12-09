HasBOLT
=======

[![Travis](https://img.shields.io/travis/zmactep/hasbolt.svg)](https://travis-ci.org/zmactep/hasbolt)
[![hackage](https://img.shields.io/hackage/v/hasbolt.svg)](https://hackage.haskell.org/package/hasbolt)
[![hackage-deps](https://img.shields.io/hackage-deps/v/hasbolt.svg)](https://hackage.haskell.org/package/hasbolt)

Haskell driver for Neo4j 3+ (BOLT protocol)

Documentation
-------------

To build Haddock documentation run:
```bash
$ stack haddock
```

Usage example
-------------

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Database.Bolt

import Data.Default
import Data.Text
import Control.Monad
import Control.Monad.Except

nineties :: BoltActionT IO [Text]
nineties = do records <- query "MATCH (nineties:Movie) WHERE nineties.released >= 1990 AND nineties.released < 2000 RETURN nineties.title"
              forM records $ \record -> record `at` "nineties.title"

genericABN :: RecordValue a => Text -> BoltActionT IO [a]
genericABN name = do toms' <- queryP "MATCH (tom:Person) WHERE tom.name CONTAINS {name} RETURN tom" (props ["name" =: name])
                     nodes <- forM toms' $ \record -> record `at` "tom"
                     forM nodes $ \node -> nodeProps node `at` "name"

actorsByNameYear :: Text -> Int -> BoltActionT IO [Node]
actorsByNameYear name year = do toms' <- queryP "MATCH (n:Person {name: {props}.name, born: {props}.born}) RETURN n" $ props ["props" =: props ["name" =: name, "born" =: year]]
                                forM toms' $ \record -> record `at` "n"

actorsByName :: Text -> BoltActionT IO [Text]
actorsByName = genericABN

wrongType :: Text -> BoltActionT IO [Int]
wrongType = genericABN 

typoInRequest :: Text -> BoltActionT IO [Text]
typoInRequest name = do toms' <- queryP "MATCH (tom:Person) WHERE tom.name CONTAINS {name} RETURN not_tom" (props ["name" =: name])
                        nodes <- forM toms' $ \record -> record `at` "tom"
                        forM nodes $ \node -> nodeProps node `at` "name"

typoInField :: Text -> BoltActionT IO [Text]
typoInField name = do toms' <- queryP "MATCH (tom:Person) WHERE tom.name CONTAINS {name} RETURN tom" (props ["name" =: name])
                      nodes <- forM toms' $ \record -> record `at` "not_tom"
                      forM nodes $ \node -> nodeProps node `at` "name"

main :: IO ()
main = do pipe <- connect $ def { user = "neo4j", password = "12345" }
          -- Prints nineties example from Movies tutorial
          putStrLn "Movies (nineties):" 
          titles <- run pipe nineties 
          forM_ titles print
          -- Prints all actors called Tom 
          putStrLn "\nActors (Tom):"
          toms' <- run pipe $ actorsByName "Tom" 
          forM_ toms' print
          -- Prints Tom Hanks 
          putStrLn "\nNodes (Tom Cruise):"
          nodes' <- run pipe $ actorsByNameYear "Tom Cruise" 1962 
          forM_ nodes' print
           -- Prints an error as type is wrong 
          putStrLn "\nWrong return type:"
          wtype' <- run pipe $ wrongType "Tom" `catchError` \e@(WrongMessageFormat _) -> liftIO (print e) >> pure [] 
          forM_ wtype' print
          -- Prints an error as the request contains a typo
          putStrLn "\nTypo in request:"
          typor' <- run pipe $ typoInRequest "Tom" `catchError` \(ResponseError e) -> liftIO (print e) >> pure [] 
          forM_ typor' print
          -- Prints an error as the field name contains a typo
          putStrLn "\nTypo in field:"
          typof' <- run pipe $ typoInField "Tom" `catchError` \e@(RecordHasNoKey _) -> liftIO (print e) >> pure [] 
          forM_ typof' print
          close pipe
```

Notes
-----

* Do not forget to import `Data.Default` to obtain default connection configuration.
* `OverloadedStrings` are very welcome, as the library doesn't use `String`s at all.
* You can use `Database.Bolt.Lazy` to work with lazy IO. In this case do not forget to read all the records before you send a next query.
* See [`test/TransactionSpec.hs`](https://github.com/zmactep/hasbolt/blob/master/test/TransactionSpec.hs) for an example of transactions usage.
* Feel free to implement your own serialization procedures with `Database.Bolt.Serialization` module import.
* Pipes work great with [resource-pool](https://hackage.haskell.org/package/resource-pool).
* For neo4j 3.4+ use `version = 2` in connection configuration. This lets you to use [new datatypes](#new-types).
* You can use both syntax to create properties maps: `fromList [("born", I 1962)]` or `props ["born" =: 1962]`.
* Note that you have to make a type hint for `Text` values in the second construction, as Haskell cannot deduce it on its own.

New types
---------

Neo4j 3.4+ implements BOLT v2 protocol (that still doesn't have any specification). Code inspection of [neo4j sources](https://github.com/neo4j/neo4j) led me to these new data types in v2. All of them are just structures with different signatures and fields.
* Point2D
```haskell
signature = 'X'
fields = { crs :: CoordinateReferenceSystem
         , x   :: Double
         , y   :: Double
         }
```
* Point3D
```haskell
signature = 'Y'
fields = { crs :: CoordinateReferenceSystem
         , x   :: Double
         , y   :: Double
         , z   :: Double
         }
```
* Duration
```haskell
signature = 'E'
fields = { months  :: Integer
         , days    :: Integer
         , seconds :: Integer
         , nanos   :: Integer
         }
```
* Date
```haskell
signature = 'D'
fields = { epochDays :: Integer
         }
```
* Time
```haskell
signature = 'T'
fields = { nanosOfDayLocal :: Integer
         , offsetSeconds   :: Integer
         }
```
* LocalTime
```haskell
signature = 't'
fields = { nanosOfDay :: Integer
         }
```
* LocalDateTime
```haskell
signature = 'd'
fields = { epochSeconds :: Integer
         , nano         :: Integer
         }
```
* DateTimeWithZoneOffset
```haskell
signature = 'F'
fields = { epochSecondsLocal :: Integer
         , nano              :: Integer
         , offsetSeconds     :: Integer
         }
```
* DateTimeWithZoneName
```haskell
signature = 'f'
fields = { epochSecondsLocal :: Integer
         , nano              :: Integer
         , zoneId            :: Integer
         }
```

Codes of Coordinate Reference Systems:
* Cartesian 2D — 7203
* Cartesian 3D — 9157
* WGS-84 2D — 4326
* WGS-84 3D — 4979

### Example

```haskell
λ> pipe <- connect $ def { user = "neo4j", password = "neo4j", version = 2 }
λ> records <- run pipe $ query "RETURN point(x: 1, y: 2, z: 3) as point"
λ> (head records) `at` "point"
S (Structure {signature = 89, fields = [I 9157,F 1.0,F 2.0,F 3.0]})
λ> close pipe
```

Here `89` is an [ASCII code](https://en.wikipedia.org/wiki/ASCII#Character_set) for 'Y', `9157` shows default cartesian 3d coordinate reference system.

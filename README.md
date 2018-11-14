HasBOLT
=======

[![Travis](https://img.shields.io/travis/zmactep/hasbolt.svg)](https://travis-ci.org/zmactep/hasbolt)
[![hackage](https://img.shields.io/hackage/v/hasbolt.svg)](https://hackage.haskell.org/package/hasbolt)
[![hackage-deps](https://img.shields.io/hackage-deps/v/hasbolt.svg)](https://hackage.haskell.org/package/hasbolt)

Haskell driver for Neo4j 3+ (BOLT protocol)

Documentation
-------------

To build Haddock documentation run:
```
$ stack haddock
```

Usage example
-------------

To use all the magic just import:
```
λ> import Database.Bolt
```

To create new connection use (it is highly recommended to use with [resource-pool](https://hackage.haskell.org/package/resource-pool)):
```
λ> pipe <- connect $ def { user = "neo4j", password = "neo4j" }
```

**NB!** For Neo4j 3.4+ also use `version = 2` to work with [new datatypes](#new-types).

To make query (`query` takes `Data.Text`, so I use **OverloadedStrings** here):
```
λ> records <- run pipe $ query "MATCH (n:Person) WHERE n.name CONTAINS \"Tom\" RETURN n"
```

You can also use parameters by `queryP`. You have to use `T` constructor here for text parameter, as Haskell is strong-typed language (see more about values in `Data.Value.Type`):
```
λ> records <- run pipe $ queryP "MATCH (n:Person) WHERE n.name CONTAINS {name} RETURN n" (fromList [("name", T "Tom")])
```

To obtain data from record you can use `at` and set of `exact` functions (the last one works for all possible Neo4j data, including primitive types, nodes, relationships and paths). So, you can do something like this:
```
toNode :: Monad m => Record -> m Node
toNode record = record `at` "n" >>= exact

λ> let x = head records
λ> toNode x >>= print
Node {nodeIdentity = 24, labels = ["Person"], nodeProps = fromList [("born",I 1962),("name",T "Tom Cruise")]}
```

To close connection just use:
```
λ> close pipe
```

New types
---------

Neo4j 3.4+ implements BOLT v2 protocol (that still doesn't have any specification). Code inspection of [neo4j sources](https://github.com/neo4j/neo4j) led me to these new data types in v2. All of them are just structures with different signatures and fields.
* Point2D
```
signature = 'X'
fields = { crs :: CoordinateReferenceSystem
         , x   :: Double
         , y   :: Double
         }
```
* Point3D
```
signature = 'Y'
fields = { crs :: CoordinateReferenceSystem
         , x   :: Double
         , y   :: Double
         , z   :: Double
         }
```
* Duration
```
signature = 'E'
fields = { months  :: Integer
         , days    :: Integer
         , seconds :: Integer
         , nanos   :: Integer
         }
```
* Date
```
signature = 'D'
fields = { epochDays :: Integer
         }
```
* Time
```
signature = 'T'
fields = { nanosOfDayLocal :: Integer
         , offsetSeconds   :: Integer
         }
```
* LocalTime
```
signature = 't'
fields = { nanosOfDay :: Integer
         }
```
* LocalDateTime
```
signature = 'd'
fields = { epochSeconds :: Integer
         , nano         :: Integer
         }
```
* DateTimeWithZoneOffset
```
signature = 'F'
fields = { epochSecondsLocal :: Integer
         , nano              :: Integer
         , offsetSeconds     :: Integer
         }
```
* DateTimeWithZoneName
```
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

```
λ> pipe <- connect $ def { user = "neo4j", password = "neo4j", version = 2 }
λ> records <- run pipe $ query "RETURN point(x: 1, y: 2, z: 3) as point"
λ> (head records) `at` "point"
S (Structure {signature = 89, fields = [I 9157,F 1.0,F 2.0,F 3.0]})
λ> close pipe
```

Here `89` is an [ASCII code](https://en.wikipedia.org/wiki/ASCII#Character_set) for 'Y', `9157` shows default cartesian 3d coordinate reference system.
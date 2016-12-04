HasBOLT
=======

Haskell driver for Neo4j 3+ (BOLT protocol)

Usage example
-------------

To use all the magic just import:
```
>> import Database.Bolt
```

To create new connection use (it is highly recommended to use with [resource-pool](https://hackage.haskell.org/package/resource-pool)):
```
>> pipe <- connect $ def { user = "neo4j", password = "neo4j" }
```

To make query (`query` takes `Data.Text`, so I use **OverloadedStrings** here):
```
>> records <- run pipe $ query "MATCH (n:Person) WHERE n.name CONTAINS \"Tom\" RETURN n"
```

To obtain data from record you can use `at` and set of `exact` functions (the last one works for all possible Neo4j data, including primitive types, nodes, relationships and paths). So, you can do something like this:
```
toNode :: Monad m => Record -> m Node
toNode record = record `at` "n" >>= exact

>> let x = head records
>> toNode x >>= print
Node {nodeIdentity = 24, labels = ["Person"], nodeProps = fromList [("born",I 1962),("name",T "Tom Cruise")]}
```

To close connection just use:
```
>> close pipe
```

HasBOLT
=======

Haskell driver for Neo4j 3+ (BOLT protocol)

Documentation
-------------

To build Haddock documentation run:
```
$ stack haddock
```

Usage example
-------------

**NB!** For a real-world example please see: [hasbolt-sample-app](https://github.com/zmactep/hasbolt-sample-app).


To use all the magic just import:
```
λ> import Database.Bolt
```

To create new connection use (it is highly recommended to use with [resource-pool](https://hackage.haskell.org/package/resource-pool)):
```
λ> pipe <- connect $ def { user = "neo4j", password = "neo4j" }
```

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

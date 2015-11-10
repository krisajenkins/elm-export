# Elm Export

Create Elm classes and JSON decoders from Haskell DataTypes.

## Usage

To use this library, you must first make the types you want to export implement `ToElmType`. This is easy. Just derive `Generic`, and then we can automatically generate the `ToElmType` instance. Here's an example with a `Person` type:

```haskell
{-# LANGUAGE DeriveGeneric #-}
module Db where

import GHC.Generics
import Elm

data Person =
  Person {id   :: Int
         ,name :: Maybe String}
  deriving (Show,Eq,Generic)

instance ToElmType Person
```

That's it for the type. Now you'll want to create some Elm source code:


```haskell
module Main where

import Db
import Elm
import Data.Proxy

spec :: Spec
spec = Spec ["Db", "Types"]
            ["import Json.Decode exposing (..)"
            ,"import Json.Decode.Extra exposing (apply,date)"
            ,toElmTypeSource (Proxy :: Proxy Person)
            ,toElmDecoderSource (Proxy :: Proxy Person)]

main :: IO ()
main = specsToDir "some/where/output" [spec]
```

When run, the directory `some/where/output` will be created, and under
that the Elm source file `Db.Types.elm` can be found.

All the hard work here is done by `toElmTypeSource` and
`toElmDecoderSource`. The Specs are just wrappers to make it easy to
create a whole Elm file from those important strings.

## Development

You will need [Stack](https://github.com/commercialhaskell/stack).

### Building

```sh
stack build
```

### Testing

```sh
stack test
```

## Status

Alpha. The author is using it in production, but it is not yet
expected to work for every reasonable case.

There are some Haskell datatypes that cannot be represented in
Elm. Obviously we will not support those. But there are some which are
legal Haskell and legal Elm, but we do not yet generate. Please send
examples, PRs and code-suggestions!

## License

Copyright © 2015 Kris Jenkins

Distributed under the Eclipse Public License.

# Elm Export

[![Build Status](https://travis-ci.org/krisajenkins/elm-export.svg)](https://travis-ci.org/krisajenkins/elm-export)

Create Elm classes and JSON decoders from Haskell DataTypes.

## What Kind Of Code Does It Generate?

Here's an example, generated from some of my Haskell code:

```elm
module Generated.Blog.Rest where

import Json.Decode exposing (..)

import Json.Decode.Extra exposing (date,(|:))

type alias Post =
  {postPostId : String
  ,postAccountId : String
  ,postSlug : String
  ,postTitle : String
  ,postContent : String
  ,postCreated : Date
  ,postPublished : Maybe Date}

decodePost : Decoder Post
decodePost =
  succeed Post
    |: ("postPostId" := string)
    |: ("postAccountId" := string)
    |: ("postSlug" := string)
    |: ("postTitle" := string)
    |: ("postContent" := string)
    |: ("postCreated" := date)
    |: ("postPublished" := maybe date)
```

Looks just like hand-written code, doesn't it?

## Installation

For now, elm-export requires stack. Add `elm-export` to your cabal
file's `build-depends` list, and this to your `stack.yaml` file:

```yaml
...
packages:
  ...
  - location:
      git: https://www.github.com/krisajenkins/elm-export
      commit: 2a026e4c3a93a18006595636e783955223f9b98e
...
```

## Usage

To use this library, you must first make the types you want to export
implement `ElmType`. This is easy. Just derive `Generic`, and then
we can automatically generate the `ElmType` instance for you. Here's
an example with a `Person` type:

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Db where

import GHC.Generics
import Elm

data Person =
  Person {id   :: Int
         ,name :: Maybe String}
  deriving (Show,Eq,Generic,ElmType)
```

That's it for the type. Now you'll want to write a main that generates
the Elm source code:

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
main = specsToDir [spec] "some/where/output"
```

Run this and the directory `some/where/output` will be created, and
under that the Elm source file `Db/Types.elm` will be found.

All the hard work here is done by `toElmTypeSource` and
`toElmDecoderSource`. The `Spec` code is just wrapping to make it easy
to create a complete Elm file from the meat that `ElmType` gives
you.

## Development

You will need [Stack](https://github.com/commercialhaskell/stack).

### Building

```sh
stack build
```

### Testing

```sh
stack test --file-watch
```

## Change Log

### V0.3.0.0
* Renamed `ToElmType` to `ElmType`, for brevity.

### V0.2.0.0
* Added Encoders (thanks to [Matthew Bray](https://github.com/mattjbray))

### V0.1.0.0
* Initial release.

## Status

Alpha. The author is using it in production, but it is not yet
expected to work for every reasonable case.

There are some Haskell datatypes that cannot be represented in
Elm. Obviously we will not support those. But there are some which are
legal Haskell and legal Elm, but we do not yet generate. Please send
examples, PRs and code-suggestions!

## Contributors

* [Matthew Bray](https://github.com/mattjbray)

## License

Copyright © 2015-2016 Kris Jenkins

Distributed under the Eclipse Public License.

## See Also

[Elm Bridge](https://hackage.haskell.org/package/elm-bridge) is a
different implementation of the same goal. That project uses Template
Haskell, this one uses GHC Generics.

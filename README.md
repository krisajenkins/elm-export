# Elm Export

[![Build Status](https://travis-ci.org/krisajenkins/elm-export.svg)](https://travis-ci.org/krisajenkins/elm-export)

Create Elm classes and JSON decoders from Haskell DataTypes.

## Installation

Elm Export is [available on Hackage](http://hackage.haskell.org/package/elm-export).

## Usage

To use this library, you must first make the types you want to export
implement `ElmType`. This is easy. Just derive `Generic`, and then
we can automatically generate the `ElmType` instance for you. Here's
an example with a `Person` type:

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Db where

import Elm
import GHC.Generics

data Person = Person
  { id :: Int
  , name :: Maybe String
  } deriving (Show, Eq, Generic, ElmType)
```

That's it for the type. Now you'll want to write a main that generates
the Elm source code:

```haskell
module Main where

import Data.Proxy
import Db
import Elm

spec :: Spec
spec =
  moduleSpec ["Db", "Types"] $ do
    require "Date exposing (Date)"
    renderType (Proxy :: Proxy Person)
    renderDecoder (Proxy :: Proxy Person)
    renderEncoder (Proxy :: Proxy Person)

main :: IO ()
main = specsToDir [spec] "some/where/output"
```

Run this and the directory `some/where/output` will be created, and
under that the Elm source file `Db/Types.elm` will be found.

### Required Elm Packages

The decoders we produce require these extra Elm packages installed:

```sh
elm package install NoRedInk/elm-json-decode-pipeline
elm package install krisajenkins/elm-exts
elm package install rtfeldman/elm-iso8601-date-strings
```

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

### Contributing guide

Development happens on the `devel` branch. Pull requests target this branch.

Generated Elm code adheres to the [`elm-format`][1] style.

JSON encoders and decoders match the default behavior of [Aeson][2].

[1]: https://github.com/avh4/elm-format
[2]: https://hackage.haskell.org/package/aeson

## Change Log

### V0.6.x
Updated to Elm 0.18.

### V0.5.x
???

### V0.4.x
???

### V0.3.0.0
* Renamed `ToElmType` to `ElmType`, for brevity.

### V0.2.0.0
* Added Encoders (thanks to [Matthew Bray](https://github.com/mattjbray))

### V0.1.0.0
* Initial release.

## Status

Beta. Several people are using it in production, reliably, but it is
not yet expected to work for every reasonable datatype.

There are some Haskell datatypes that cannot be represented in
Elm. Obviously we will not support those. But there are some which are
legal Haskell and legal Elm, but we do not yet generate. Please send
examples, PRs and code-suggestions!

## Contributors

* [Matthew Bray](https://github.com/mattjbray)
* [Domen Kožar](https://github.com/domenkozar)

## License

Copyright © 2015-2017 Kris Jenkins

Distributed under the Eclipse Public License.

## See Also

[Elm Bridge](https://hackage.haskell.org/package/elm-bridge) is a
different implementation of the same goal. That project uses Template
Haskell, this one uses GHC Generics.

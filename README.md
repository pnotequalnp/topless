# topless
Put the top down and enjoy the sun with convertible types

## Concept
Mimics Rust's `From` and `Into` traits, but unifying them with the `TryFrom` and `TryInto` traits.
For example with the provided instances, `from @[Int] @(NonEmpty Int) :: [Int] -> Maybe (NonEmpty
Int)`, but `from @(NonEmpty Int) @[Int] :: NonEmpty Int -> [Int]`.

## Usage

```haskell
{-# LANGUAGE OverloadedLists #-}

import Data.List.NonEmpty (NonEmpty)
import Topless (from, into)

xs :: [Int]
xs = [1..5]

xs' :: Maybe (NonEmpty Int)
xs' = into @(NonEmpty Int) xs
-- xs' = Just [1, 2, 3, 4, 5]

ys :: NonEmpty Int
ys = [1..5]

ys' :: [Int]
ys' = into @[Int] ys
-- ys' = [1, 2, 3, 4, 5]

zs :: [Int]
zs = []

zs' :: Maybe (NonEmpty Int)
zs' = into @(NonEmpty Into) zs
-- zs' = Nothing
```

## Writing Instances
You can use a custom error type. you will need `MultiParamTypeClasses` and `TypeFamilies`. For
example, conversion from a `Double` to a hypothetical `Natural` type:

```haskell
data DoubleNatError
  = Negative
  | NonIntegral

instance Convertible Double Natural where
  type Error Double Natural = DoubleNatError
  -- from :: Double -> Either DoubleNatError Natural
  from = _
```

Using `Data.Void.Void` as the error type results in a plain function for `from`, using `()` will
make `from` map to a `Maybe`, and any other type `e` will make it map to an `Either e`.

## Known Issues
GHC can't infer the result type parameter, even if it's obvious, so it must always be applied. This
is annoying with `from` because it means you always have to apply both type arguments explicitly.
With `into` at least you only have to supply one type argument.

There are next to no instances included.

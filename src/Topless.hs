{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Topless where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Void (Void)

type family ReduceError e a where
  ReduceError Void a = a
  ReduceError () a = Maybe a
  ReduceError e a = Either e a

class Convertible a b where
  type Error a b :: Type
  from :: a -> ReduceError (Error a b) b

into :: forall b a. Convertible a b => a -> ReduceError (Error a b) b
into = from @a @b

instance Convertible a a where
  type Error a a = Void
  from = id

instance Convertible (NonEmpty a) [a] where
  type Error (NonEmpty a) [a] = Void
  from = NE.toList

instance Convertible [a] (NonEmpty a) where
  type Error [a] (NonEmpty a) = ()
  from = NE.nonEmpty

instance Convertible Integer Int where
  type Error Integer Int = Void
  from = fromInteger

instance Convertible Int Integer where
  type Error Int Integer = Void
  from = fromIntegral

instance Convertible (Either e a) (Maybe a) where
  type Error (Either e a) (Maybe a) = Void
  from = either (const Nothing) Just

instance Convertible Int Double where
  type Error Int Double = Void
  from = fromIntegral

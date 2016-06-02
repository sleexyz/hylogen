module Hylogen.WithHylide ( module Hylogen.WithHylide.Util
                          , module Hylogen.WithHylide.Core
                          , module Hylogen
                          , module Data.VectorSpace
                          , module Data.Function
                          ) where

import           Hylogen.WithHylide.Util
import           Hylogen.WithHylide.Core
import           Hylogen

import           Data.Function
import           Data.VectorSpace

-- | Analog of @<$>@, but for @&@
infixl 5 <&>
(<&>) :: (Functor f) => f a -> (a -> b) -> f b
(<&>) = flip fmap


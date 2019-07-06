{-# language
        BangPatterns
      , DeriveFoldable
      , DeriveFunctor
      , DeriveTraversable
      , DerivingStrategies
      , GeneralizedNewtypeDeriving
      , InstanceSigs
      , MonadComprehensions
      , ScopedTypeVariables
  #-}

module Prob where

import Data.Primitive.Array
import Data.Primitive.PrimArray
import qualified Data.Primitive.Contiguous as C

data Mapping a = Mapping !Double a
  deriving stock (Eq, Ord, Show, Read)
  deriving stock (Functor, Foldable, Traversable)

newtype Distribution a = Distribution
  { getDistribution :: Array (Mapping a)
  }

instance Functor Distribution where
  fmap f = Distribution . C.map (fmap f) . getDistribution

instance Applicative Distribution where
  pure = certainly
  (<*>) :: Distribution (a -> b) -> Distribution a -> Distribution b
  f <*> x = Distribution
    [ Mapping (q * p) (g a)
    | (Mapping p g) <- getDistribution f
    , (Mapping q a) <- getDistribution x
    ]

instance Monad Distribution where
  (>>=) :: Distribution a -> (a -> Distribution b) -> Distribution b
  d >>= f = Distribution
    [ Mapping (q * p) y
    | Mapping p x <- getDistribution d
    , Mapping q y <- getDistribution (f x)
    ]

certainly :: a -> Distribution a
certainly x = Distribution (C.singleton (Mapping 1 x))

size :: Distribution a -> Int
size = C.size . getDistribution

newtype Spread a = Spread { getSpread :: Array a -> Distribution a }

choose :: forall a. Double -> a -> a -> Distribution a
choose p x y = Distribution
  $ C.zipWith Mapping
      (C.doubleton p (1 - p) :: PrimArray Double) -- lol
      (C.doubleton x y :: Array a)

--norm'' :: Ord a => [Mapping a] -> [Mapping a]
--norm'' = map grouper . groupBy (equating

newtype Shape = Shape { getShape :: Double -> Double }

linear :: Shape
linear = Shape id

uniform :: Shape
uniform = Shape (const 1)

negExp :: Shape
negExp = Shape (exp . negate)

normal :: Shape
normal = normalCurve 0.5 0.5

normalCurve :: Double -> Double -> Shape
normalCurve mean dev = Shape $ \x ->
  let u = (x - mean) / dev
  in exp (-1/2 * u^(2::Int)) / sqrt (2 * pi)


{-|
Module      : TrueSkill.Math
Description : Implements math helper needed for learning and inference.
Copyright   : (c) Martin Kiefel, 2014
License     : BSD2
Maintainer  : mk@nopw.de
Stability   : experimental
Portability : portable
-}

module TrueSkill.Math
  ( erf
  , normPdf
  , normCdf
  , monomialGauss
  , choose
  , fac
  , doubleFac
  , monomialGaussUpper
  )
  where

import           Data.Number.Erf

import           TrueSkill.Autodiff


-- | Binomial coefficient.
choose :: Int -> Int -> Int
choose _ 0 = 1
choose 0 _ = 0
choose n k = choose (n-1) (k-1) * n `div` k

-- | Cummulative density function of a normal Gaussion distribution.
normCdf :: (Floating d, Ord d, Erf d) => d -> d
normCdf x = 0.5 * (1 + erf (x / sqrt 2))
{-# SPECIALISE normCdf :: Double -> Double #-}
{-# SPECIALISE normCdf :: AD -> AD #-}

-- | Probability density function of a normal Gaussian distribution.
normPdf :: Floating d => d -> d
normPdf x = exp (-x^(2 :: Int) / 2) / sqrt (2 * pi)
{-# SPECIALISE normPdf :: Double -> Double #-}
{-# SPECIALISE normPdf :: AD -> AD #-}

-- | Faculty.
fac :: Int -> Int
fac 0 = 1
fac n = product [1..n]

-- | Double faculty.
doubleFac :: Int -> Int
doubleFac 0 = 1
doubleFac n = product [n - 2*i | i <- [0 .. (n + 1) `div` 2 - 1]]

-- | Non-normalized normal.
gauss :: Floating d => d -> d
gauss x = exp (-0.5 * x^(2 :: Int))

-- | Integral primitiv for \int \gauss(x) \dx.
normPrimitive :: (Floating d, Ord d, Erf d) => d -> d
normPrimitive x = normCdf x * sqrt (2 * pi)
{-# SPECIALISE normPrimitive :: Double -> Double #-}
{-# SPECIALISE normPrimitive :: AD -> AD #-}

-- | Integral primitiv for \int x \gauss(x) \dx.
firstPrimitive :: Floating d => d -> d
firstPrimitive x = -gauss x
{-# SPECIALISE firstPrimitive :: Double -> Double #-}
{-# SPECIALISE firstPrimitive :: AD -> AD #-}

-- | Integral primitiv for \int x^2 \gauss(x) \dx.
secondPrimitive :: (Floating d, Ord d, Erf d) => d -> d
secondPrimitive x = normCdf x * sqrt (2 * pi) - x * gauss x
{-# SPECIALISE secondPrimitive :: Double -> Double #-}
{-# SPECIALISE secondPrimitive :: AD -> AD #-}

-- | Integral primitiv for \int x^{2*i+1} \gauss(x) \dx.
oddPrimitive :: Floating d => Int -> d -> d
oddPrimitive i x =
    -gauss x
    * sum [fromIntegral (doubleFac (2 * i))
           / fromIntegral (doubleFac (2 * j)) * x ^ (2 * j)
          | j <- [0 .. i]]
{-# SPECIALISE oddPrimitive :: Int -> Double -> Double #-}
{-# SPECIALISE oddPrimitive :: Int -> AD -> AD #-}

-- | Integral primitiv for \int x^{2*i+2} \gauss(x) \dx.
evenPrimitive :: (Floating d, Ord d, Erf d) => Int -> d -> d
evenPrimitive i x =
    -gauss x
    * sum [fromIntegral (doubleFac (2 * i + 1))
           / fromIntegral (doubleFac (2 * j + 1)) * x ^ (2 * j + 1)
          | j <- [0 .. i]]
    + fromIntegral (doubleFac (2 * i + 1)) * normCdf x * sqrt(2*pi)
{-# SPECIALISE evenPrimitive :: Int -> Double -> Double #-}
{-# SPECIALISE evenPrimitive :: Int -> AD -> AD #-}

-- | Convenience functions for integrals of monomials and Gaussians.
monomialGauss :: (Floating d, Ord d, Erf d) => Int -> d -> d -> d
monomialGauss 0 lower upper = normPrimitive upper - normPrimitive lower
monomialGauss 1 lower upper = firstPrimitive upper - firstPrimitive lower
monomialGauss 2 lower upper = secondPrimitive upper - secondPrimitive lower
monomialGauss n lower upper
    | odd n     = oddPrimitive n' upper - oddPrimitive n' lower
    | otherwise = evenPrimitive (n' - 1) upper - evenPrimitive (n' - 1) lower
    where n' = n `div` 2
{-# SPECIALISE monomialGauss :: Int -> Double -> Double -> Double #-}
{-# SPECIALISE monomialGauss :: Int -> AD -> AD -> AD #-}

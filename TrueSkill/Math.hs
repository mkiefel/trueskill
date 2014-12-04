{-|
Module      : TrueSkill.Math
Description : Implements math helper needed for learning and inference.
Copyright   : (c) Martin Kiefel, 2014
License     : BSD2
Maintainer  : mk@nopw.de
Stability   : experimental
Portability : portable
-}

module TrueSkill.Math where

-- | Direct implemntation of error function to make it differentiable.
-- Taken from http://en.wikipedia.org/wiki/Error_function
erf :: (Floating d, Ord d) => d -> d
erf x
    | x < 0     = -erf(-x)
    | otherwise = 1 - 1 / (1 + a1*x + a2*x**2 + a3*x**3 + a4*x**4)**4
  where
    a1 = 0.278393
    a2 = 0.230389
    a3 = 0.000972
    a4 = 0.078108

-- | Cummulative density function of a normal Gaussion distribution.
normcdf :: (Floating d, Ord d) => d -> d
normcdf x = 0.5 * (1 + erf (x / sqrt 2))

-- | Probability density function of a normal Gaussian distribution.
normpdf :: Floating d => d -> d
normpdf x = exp (-x**2 / 2) / sqrt (2 * pi)


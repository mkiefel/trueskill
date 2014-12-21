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
  )
  where

-- | Binomial coefficient.
choose :: Int -> Int -> Int
choose n 0 = 1
choose 0 k = 0
choose n k = choose (n-1) (k-1) * n `div` k

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
normCdf :: (Floating d, Ord d) => d -> d
normCdf x = 0.5 * (1 + erf (x / sqrt 2))

-- | Probability density function of a normal Gaussian distribution.
normPdf :: Floating d => d -> d
normPdf x = exp (-x**2 / 2) / sqrt (2 * pi)

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
gauss x = exp (-0.5 * x**2)

-- | Integral primitiv for \int \gauss(x) \dx.
normPrimitive :: (Floating d, Ord d) => d -> d
normPrimitive x = normCdf x * sqrt (2 * pi)

-- | Integral primitiv for \int x \gauss(x) \dx.
firstPrimitive :: Floating d => d -> d
firstPrimitive x = -gauss x

-- | Integral primitiv for \int x^2 \gauss(x) \dx.
secondPrimitive :: (Floating d, Ord d) => d -> d
secondPrimitive x = normCdf x * sqrt (2 * pi) - x * gauss x

-- | Integral primitiv for \int x^{2*i+1} \gauss(x) \dx.
oddPrimitive :: Floating d => Int -> d -> d
oddPrimitive i x =
    -gauss x
    * sum [fromIntegral (doubleFac (2 * i))
        / fromIntegral (doubleFac (2 * j)) * x ** (2 * fromIntegral j)
        | j <- [0 .. i]]

-- | Integral primitiv for \int x^{2*i+2} \gauss(x) \dx.
evenPrimitive :: (Floating d, Ord d) => Int -> d -> d
evenPrimitive i x =
    -gauss x
    * sum [fromIntegral (doubleFac (2 * i + 1))
        / fromIntegral (doubleFac (2 * j + 1)) * x ** (2 * fromIntegral j + 1)
        | j <- [0 .. i]]
    + fromIntegral (doubleFac (2 * i + 1)) * normCdf x * sqrt(2*pi)

-- | Convenience functions for integrals of monomials and Gaussians.
monomialGauss :: (Floating d, Ord d) => Int -> d -> d -> d
monomialGauss 0 lower upper = normPrimitive upper - normPrimitive lower
monomialGauss 1 lower upper = firstPrimitive upper - firstPrimitive lower
monomialGauss 2 lower upper = secondPrimitive upper - secondPrimitive lower
monomialGauss n lower upper
    | odd n     = oddPrimitive n' upper - oddPrimitive n' lower
    | otherwise = evenPrimitive (n' - 1) upper - evenPrimitive (n' - 1) lower
    where n' = n `div` 2

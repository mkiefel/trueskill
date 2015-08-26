{-# LANGUAGE DataKinds, TemplateHaskell #-}
module TrueSkill.Autodiff where

import           Linear  hiding ( trace )

import           Data.Number.Erf

import           Debug.Trace

data AD = AD
          {
            getValue :: !Double
          , getGradient :: !(V2 Double)
          }
        deriving Show

lift :: Double -> AD
lift d = AD d zero

makeAD :: Double -> Int -> AD
makeAD d m = AD d g
  where
    g
      | m == 0    = V2 1 0
      | m == 1    = V2 0 1
      | otherwise = V2 0 0

instance Eq (AD) where
  (==) (AD d _) (AD e _) = d == e

instance Ord (AD) where
  compare (AD d _) (AD e _) = d `compare` e

instance Num (AD) where
  (+) (AD d v) (AD e w) = AD (d + e) (v + w)
  (*) (AD d v) (AD e w) = AD (d * e) (d*^w + e*^v)
  (-) (AD d v) (AD e w) = AD (d - e) (v - w)
  negate (AD d v) = AD (-d) $ negated v
  abs = trace "abs" undefined
  signum = trace "signum" undefined
  fromInteger i = AD (fromInteger i) zero

instance Erf (AD) where
  erf (AD d v) = AD (erf d) ((2 / sqrt pi * exp (-d^(2 :: Int))) *^ v)

instance Fractional (AD) where
  (/) (AD d v) (AD e w) = AD (d / e)
                          (v ^/ e - w ^* (d / (e * e)))

  recip = trace "recip" undefined
  fromRational r = AD (fromRational r) zero

instance Floating (AD) where
  pi = lift pi
  exp (AD d v) = AD d' (d'*^v)
    where
      d' = exp d

  sqrt (AD d v) = AD d' ((0.5 / d') *^ v)
    where
      d' = sqrt d
  log (AD d v) = AD (log d) (v ^/ d)
  (**) = trace "**" undefined
  logBase = trace "logBase" undefined
  sin = trace "sin" undefined
  tan = trace "tan" undefined
  cos = trace "cos" undefined
  asin = trace "asin" undefined
  atan = trace "atan" undefined
  acos = trace "acos" undefined
  sinh = trace "sinh" undefined
  tanh = trace "tanh" undefined
  cosh = trace "cosh" undefined
  asinh = trace "asinh" undefined
  atanh = trace "atanh" undefined
  acosh = trace "acosh" undefined

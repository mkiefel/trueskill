{-|
Module      : TrueSkill.Poisson
Description : Implements Poisson distribution factor for EP.
Copyright   : (c) Martin Kiefel, 2014
License     : BSD2
Maintainer  : mk@nopw.de
Stability   : experimental
Portability : portable
-}

module TrueSkill.Poisson
  ( epMessage, predictionMessage )
  where

import           Control.Lens ( (^.) )
import           Data.Number.Erf

import           TrueSkill.Message
import           TrueSkill.Math ( choose
                                , monomialGaussUpper
                                , fac
                                )
import           TrueSkill.Autodiff

import           Debug.Trace

-- | Implements the integral of
-- \int x^k \exp (-0.5 \pi (x - (\tau - 1)/\pi)^2) \dx
-- for any k (the observation of the Poisson distribution)
-- and any pi_ and tau (the natural parameters of the incoming
-- Gaussian message.
--
-- The Gaussian message is defined to be
integral :: (Floating d, Ord d, Erf d) => Int -> d -> d -> d
integral k pi_' tau' =
    sum [fromIntegral (choose k i)
         * (tau' - 1) ^ (k - i)
         / sqrt pi_' ^ (2*k - i + 1)
         * monomialGaussUpper i (-(tau' - 1) / sqrt pi_')
        | i <- [0 .. k]]
{-# SPECIALISE integral :: Int -> Double -> Double -> Double #-}
{-# SPECIALISE integral :: Int -> AD -> AD -> AD #-}

predictionMessage :: (Floating d, Ord d, Erf d) => Message d -> [d]
predictionMessage message = map ( / partition ) distribution
  where
    pi_' = message ^. pi_
    tau' = message ^. tau

    distribution = take 10 [ integral k pi_' tau' / fromIntegral (fac k)
                           | k <- [0..] ]
    partition = sum distribution
{-# SPECIALISE predictionMessage :: Message Double -> [Double] #-}
{-# SPECIALISE predictionMessage :: Message AD -> [AD] #-}

-- | Approximates the state distribution of a variable including the incoming
-- Gaussian message and the observation of a Poisson factor attached to it.
epMessage :: (Floating d, Ord d, Erf d) => Int -> Message d -> Message d
epMessage k message
  | newPi_ < pi_' = trace "epMessage" undefined
  | otherwise =
      Message { _pi_ = newPi_
              , _tau = newTau
              }
    where
        pi_' = message ^. pi_
        tau' = message ^. tau

        partition = integral k pi_' tau'
        expectedX = integral (k+1) pi_' tau' / partition
        expectedX2 = integral (k+2) pi_' tau' / partition

        mu = expectedX
        sigma2 = expectedX2 - expectedX^(2 :: Int)

        -- Map the expected sufficient statistics back to natural parameters.
        newPi_ = 1 / sigma2
        newTau = mu / sigma2
{-# SPECIALISE epMessage :: Int -> Message Double -> Message Double #-}
{-# SPECIALISE epMessage :: Int -> Message AD -> Message AD #-}

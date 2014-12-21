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

import           TrueSkill.Message
import           TrueSkill.Math ( choose
                                , monomialGauss
                                , fac
                                )
-- | Implements the integral of
-- \int x^k \exp (-0.5 \pi (x - (\tau - 1)/\pi)^2) \dx
-- for any k (the observation of the Poisson distribution)
-- and any pi_ and tau (the natural parameters of the incoming
-- Gaussian message.
--
-- The Gaussian message is defined to be
integral :: (Floating d, Ord d) => Int -> d -> d -> d
integral k pi_ tau =
    sum [fromIntegral (choose k i)
         * (tau - 1) ** fromIntegral (k - i)
         * pi_ ** (0.5 * fromIntegral (-2*k + i - 1))
         * monomialGauss i (-(tau - 1) / sqrt pi_) 20
         | i <- [0 .. k]]

predictionMessage :: (Floating d, Ord d) => Message d -> [d]
predictionMessage message = map ( / partition ) distribution
  where
    pi_' = message ^. pi_
    tau' = message ^. tau

    distribution = take 10 [ integral k pi_' tau' / fromIntegral (fac k) | k <- [0..] ]
    partition = sum distribution

-- | Approximates the state distribution of a variable including the incoming
-- Gaussian message and the observation of a Poisson factor attached to it.
epMessage :: (Floating d, Ord d) => Int -> Message d -> Message d
epMessage k message =
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
        sigma2 = expectedX2 - expectedX**2

        -- Map the expected sufficient statistics back to natural parameters.
        newPi_ = 1 / sigma2
        newTau = mu / sigma2

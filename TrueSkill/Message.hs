{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : TrueSkill.Message
Description : Implements Gaussian messages.
Copyright   : (c) Martin Kiefel, 2014
License     : BSD2
Maintainer  : mk@nopw.de
Stability   : experimental
Portability : portable
-}

module TrueSkill.Message where

import           Control.Lens
import           Data.Default
import           Control.DeepSeq

-- | Message represents a parameteric message, which is sent between the random
-- variables in the factor graph.
--
-- Messages are parametrized by the sufficient statistics of a Gaussian
-- distribution.
-- Like so: exp(\tau x - 0.5 \pi x^2)
data Message d = Message
  { _pi_ :: !d
  , _tau :: !d
  }
makeLenses ''Message

instance Floating d => Default (Message d) where
    def = Message { _pi_ = 0.0, _tau = 0.0 }

instance NFData (Message d) where
    rnf (Message pi_' tau') = pi_' `seq` tau' `seq` ()

-- | Translates the natural parameters of a message to the more readable
-- standard parameters for a Gaussian distribution -- mean and variance.
toMuSigma2 :: Floating d => Message d -> (d, d)
toMuSigma2 msg = (mu, sigma2)
  where
    sigma2 = 1.0 / msg^.pi_
    mu = msg^.tau * sigma2

-- | Translates the standard parameters for a Gaussian distribution
-- (mean and variance) to the a corresponding message.
fromMuSigma2 :: Floating d => d -> d -> Message d
fromMuSigma2 mu sigma2 =
    Message { _pi_ = 1.0 / sigma2
            , _tau = mu / sigma2 }

instance (Show d, Floating d) => Show (Message d) where
  show m = "Message (" ++ show mu ++ ", " ++ show sigma2 ++ ")"
    where
      (mu, sigma2) = toMuSigma2 m

-- | Include a message in a belief.
include :: Floating d => Message d -> Message d -> Message d
include stateLeft stateRight =
    Message
      { _pi_  = stateLeft^.pi_ + stateRight^.pi_
      , _tau = stateLeft^.tau + stateRight^.tau
      }

-- | Remove a message from a belief.
exclude :: Floating d => Message d -> Message d -> Message d
exclude stateLeft stateRight =
    Message
      { _pi_ = stateLeft^.pi_ - stateRight^.pi_
      , _tau = stateLeft^.tau - stateRight^.tau
      }


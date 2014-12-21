{-|
Module      : TrueSkill
Description : TrueSkill implementation.
Copyright   : (c) Martin Kiefel, 2014
License     : BSD2
Maintainer  : mk@nopw.de
Stability   : experimental
Portability : portable
-}

module TrueSkill
  -- Model
  ( predict
  , train
  , offense
  , defense
  , sigmaOffense
  , sigmaDefense
  , skills
  , makeSkills
  , Parameter(..)
  , Player
  , Result(..)
  -- Message
  , toMuSigma2
  , fromMuSigma2
  , Message
  -- Poisson
  , predictionMessage
  )
  where

import           TrueSkill.Model ( predict
                                 , train
                                 , offense
                                 , defense
                                 , sigmaOffense
                                 , sigmaDefense
                                 , skills
                                 , makeSkills
                                 , Parameter(..)
                                 , Player
                                 , Skills
                                 , Result(..)
                                 )
import           TrueSkill.Message ( toMuSigma2
                                   , fromMuSigma2
                                   , Message
                                   )
import           TrueSkill.Poisson ( predictionMessage )

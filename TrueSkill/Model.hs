{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : TrueSkill.Model
Description : Implements TrueSkill, a team ranking inference algorithm.
Copyright   : (c) Martin Kiefel, 2014
License     : BSD2
Maintainer  : mk@nopw.de
Stability   : experimental
Portability : portable

See http://machinelearning.wustl.edu/mlpapers/paper_files/NIPS2006_688.pdf
-}

module TrueSkill.Model
  where

import           Control.Lens
import qualified Data.HashMap.Strict as M
import           Data.Default
import           Control.DeepSeq
import           Data.List ( foldl' )

import           TrueSkill.Message
import qualified TrueSkill.Poisson as Poisson
import           TrueSkill.Autodiff


-- | The model parameters are gathered in this structure.
data Parameter d = Parameter
  { _sigmaOffense :: !d
  , _sigmaDefense :: !d
  }
makeLenses ''Parameter

type GameID = Int

data Skills d = Skills
  { _offense :: Message d
  , _defense :: Message d
  } deriving Show
makeLenses ''Skills

makeSkills :: Message d -> Message d -> Skills d
makeSkills offense' defense' = Skills
    { _offense = offense'
    , _defense = defense'
    }

instance Floating d => Default (Skills d) where
    def = Skills { _offense = def
                 , _defense = def
                 }
instance NFData (Skills d) where
    rnf (Skills o d) = rnf o `seq` rnf d

includeSkills :: Floating d => Skills d -> Skills d -> Skills d
includeSkills s t = offense %~ (`include` (t^.offense)) $
                    defense %~ (`include` (t^.defense)) $ s
{-# SPECIALISE includeSkills :: Skills Double -> Skills Double
                             -> Skills Double #-}
{-# SPECIALISE includeSkills :: Skills AD -> Skills AD
                             -> Skills AD #-}

excludeSkills :: Floating d => Skills d -> Skills d -> Skills d
excludeSkills s t = offense %~ (`exclude` (t^.offense)) $
                    defense %~ (`exclude` (t^.defense)) $ s
{-# SPECIALISE excludeSkills :: Skills Double -> Skills Double
                             -> Skills Double #-}
{-# SPECIALISE excludeSkills :: Skills AD -> Skills AD
                             -> Skills AD #-}

data Player d = Player
  { _games :: M.HashMap GameID (Skills d) -- ^ Includes all games that were
                                          --   used to infer the skill.
  , _skills :: Skills d
  } deriving Show
makeLenses ''Player

instance Floating d => Default (Player d) where
  def = Player { _games = M.empty
               , _skills = def
               }

newtype Result = Result (Int, Int)
  deriving (Show, Eq)


instance Floating d => Default (Parameter d) where
  def = Parameter { _sigmaOffense = 0.1
                  , _sigmaDefense = 0.1
                  }

fuse3 :: (t1 -> t2 -> t3 -> s) -> (t1, t1) -> (t2, t2) -> (t3, t3)
      -> (s, s)
fuse3 f (a, a_) (b, b_) (c, c_) = (f a b c, f a_ b_ c_)

fuse2 :: (t1 -> t2 -> s) -> (t1, t1) -> (t2, t2) -> (s, s)
fuse2 f (a, a_) (b, b_) = (f a b, f a_ b_)

-- | Updates the skills of a set of players given a game.
train :: (Floating d, Ord d)
    => Parameter d -> GameID -> Result -> ([Player d], [Player d])
    -> ([Player d], [Player d])
train parameter gameID result players =
    fuse3 update sentSkills' recvSkills players
  where
    update :: Floating d => [Skills d] -> [Skills d] -> [Player d]
           -> [Player d]
    update = zipWith3 (\s m p -> games %~ M.insert gameID m
               $ skills .~ (s `includeSkills` m) $ p)

    recvSkills = treePass parameter result sentSkills'

    sentSkills' = both %~ map sentSkills $ players

    sentSkills player = excludeSkills p g
      where
        p = player ^. skills
        g = M.lookupDefault def gameID (view games player)
{-# SPECIALISE train :: Parameter Double -> GameID -> Result
                     -> ([Player Double], [Player Double])
                     -> ([Player Double], [Player Double]) #-}
{-# SPECIALISE train :: Parameter AD -> GameID -> Result
                     -> ([Player AD], [Player AD])
                     -> ([Player AD], [Player AD]) #-}

-- | Calculates the Gaussian belief of a game result.
predict :: Floating d => Parameter d -> ([Player d], [Player d])
        -> (Message d, Message d)
predict parameter players = toDifferenceMsgs
  where
    sentSkills' = both.traverse %~ view skills $ players

    performanceMsgs1 = ( performanceMsgs ^. _1 . offense
                       , performanceMsgs ^. _2 . defense
                       )
    performanceMsgs2 = ( performanceMsgs ^. _2 . offense
                       , performanceMsgs ^. _1 . defense
                       )
    toDifferenceMsgs = ( toDifference performanceMsgs1
                       , toDifference performanceMsgs2
                       )

    performanceMsgs =
        both %~ (\s -> makeSkills
          (fromPerformance (traverse %~ view offense $ s))
          (fromPerformance (traverse %~ view defense $ s)))
        $ skillMsgs

    skillMsgs = mapSkillMsgs (fromSkill (parameter^.sigmaOffense))
                             (fromSkill (parameter^.sigmaDefense)) sentSkills'
{-# SPECIALISE predict :: Parameter Double -> ([Player Double], [Player Double])
                       -> (Message Double, Message Double) #-}
{-# SPECIALISE predict :: Parameter AD -> ([Player AD], [Player AD])
                       -> (Message AD, Message AD) #-}

-- | A complete message pass down to the observed result variable and back.
treePass :: (Floating d, Ord d)
    => Parameter d -> Result -> ([Skills d], [Skills d])
    -> ([Skills d], [Skills d])
treePass parameter (Result result) playerSkills =
    mapSkillMsgs (toSkill (parameter^.sigmaOffense))
                 (toSkill (parameter^.sigmaDefense))
    toPerformanceMsgs
  where
    toPerformanceMsgs =
        deepseq fromDifferenceMsgs $ fuse2 go skillMsgs fromDifferenceMsgs
      where
        go skills' fromDifferenceSkills = zipWith makeSkills offenses defenses
          where
            mangle s = toPerformance (traverse %~ view s $ skills')
                         (fromDifferenceSkills ^. s)

            offenses = mangle offense
            defenses = mangle defense

    fromDifferenceMsgs =
        ( makeSkills offense1 defense1
        , makeSkills offense2 defense2
        )
      where
        (offense1, defense2) = fromDifference (fromResult ^. _1) performanceMsgs1
        (offense2, defense1) = fromDifference (fromResult ^. _2) performanceMsgs2

        fromResult = fuse2 exclude marginals toDifferenceMsgs

    marginals = deepseq toDifferenceMsgs
                $ fuse2 Poisson.epMessage result toDifferenceMsgs

    performanceMsgs1 = ( performanceMsgs ^. _1 . offense
                       , performanceMsgs ^. _2 . defense
                       )
    performanceMsgs2 = ( performanceMsgs ^. _2 . offense
                       , performanceMsgs ^. _1 . defense
                       )
    toDifferenceMsgs = ( toDifference performanceMsgs1
                       , toDifference performanceMsgs2
                       )

    performanceMsgs =
        both %~ (\s -> makeSkills
          (fromPerformance (traverse %~ view offense $ s))
          (fromPerformance (traverse %~ view defense $ s)))
        $ skillMsgs

    skillMsgs = mapSkillMsgs (fromSkill (parameter^.sigmaOffense))
                             (fromSkill (parameter^.sigmaDefense)) playerSkills
{-# SPECIALISE treePass :: Parameter Double -> Result
                        -> ([Skills Double], [Skills Double])
                        -> ([Skills Double], [Skills Double]) #-}
{-# SPECIALISE treePass :: Parameter AD -> Result
                        -> ([Skills AD], [Skills AD])
                        -> ([Skills AD], [Skills AD]) #-}

mapSkillMsgs :: (Message d -> Message d)
             -> (Message d -> Message d)
             -> ([Skills d], [Skills d])
             -> ([Skills d], [Skills d])
mapSkillMsgs fOffense fDefense =
    both.traverse %~
    ( (offense %~ fOffense)
    . (defense %~ fDefense)
    )

fromSkill :: Floating d => d -> Message d -> Message d
fromSkill beta msg = Message
    { _pi_  = a * msg^.pi_
    , _tau = a * msg^.tau
    }
  where
    a = 1.0 / (1.0 + c2 * msg^.pi_)
    c2 = beta ^ (2 :: Int)
{-# SPECIALISE fromSkill :: Double -> Message Double -> Message Double #-}
{-# SPECIALISE fromSkill :: AD -> Message AD -> Message AD #-}

-- | Pass of a weighted sum.
weightedPass :: Floating d => [(d, Message d)] -> Message d
weightedPass msgs = Message
    { _pi_ = piNew
    , _tau = tauNew
    }
  where
    (invPiNew, preTau) = foldl' go (0, 0) msgs
    go (p, t) (a, m) =
      p' `seq` t' `seq` (p', t')
      where
        p' = p + a^(2 :: Int) / pi_'
        t' = t + a * tau' / pi_'
        pi_' = m^.pi_
        tau' = m^.tau

    piNew = 1.0 / invPiNew
    tauNew = piNew * preTau
{-# SPECIALISE weightedPass :: [(Double, Message Double)] -> Message Double #-}
{-# SPECIALISE weightedPass :: [(AD, Message AD)] -> Message AD #-}

-- | Calculates the belief of the team skill given player skills.
fromPerformance :: Floating d => [Message d] -> Message d
fromPerformance msgs = weightedPass $ zip (repeat 1) msgs
{-# SPECIALISE fromPerformance :: [Message Double] -> Message Double #-}
{-# SPECIALISE fromPerformance :: [Message AD] -> Message AD #-}

toDifference :: Floating d => (Message d, Message d) -> Message d
toDifference (performanceLeftMsg, performanceRightMsg) =
    weightedPass [(1, performanceLeftMsg), (-1, performanceRightMsg)]
{-# SPECIALISE toDifference :: (Message Double, Message Double)
                            -> Message Double #-}
{-# SPECIALISE toDifference :: (Message AD, Message AD)
                            -> Message AD #-}

-- | Calculates the messages from the difference random variable back up to the
-- team variables.
fromDifference :: Floating d => Message d -> (Message d, Message d)
               -> (Message d, Message d)
fromDifference toDifferenceFactorMsg (performanceLeftMsg, performanceRightMsg) =
    ( weightedPass [(1, performanceRightMsg), (1, toDifferenceFactorMsg)]
    , weightedPass [(1, performanceLeftMsg), (-1, toDifferenceFactorMsg)]
    )
{-# SPECIALISE fromDifference :: Message Double
                              -> (Message Double, Message Double)
                              -> (Message Double, Message Double) #-}
{-# SPECIALISE fromDifference :: Message AD
                              -> (Message AD, Message AD)
                              -> (Message AD, Message AD) #-}

-- | Given the message from the difference random variable this function
-- calculates the messages to each of the involved player game skill variables
-- of a team.
toPerformance :: Floating d => [Message d] -> Message d -> [Message d]
toPerformance fromPerformanceMsgs msg = go [] fromPerformanceMsgs
  where
    go _ []                    = []
    go headMsgs (m : tailMsgs) = weightedPass ((1, msg)
                                 : zipM1 headMsgs
                                 ++ zipM1 tailMsgs)
        : go (m : headMsgs) tailMsgs

    zipM1 msgs = zip (repeat (-1)) msgs
{-# SPECIALISE toPerformance :: [Message Double] -> Message Double
                             -> [Message Double] #-}
{-# SPECIALISE toPerformance :: [Message AD] -> Message AD
                             -> [Message AD] #-}

-- | Message for a player skill variable given its corresponding game variable.
toSkill :: Floating d => d -> Message d -> Message d
toSkill beta = fromSkill beta
{-# SPECIALISE toSkill :: Double -> Message Double -> Message Double #-}
{-# SPECIALISE toSkill :: AD -> Message AD -> Message AD #-}

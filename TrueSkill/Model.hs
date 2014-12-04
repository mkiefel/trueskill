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
import qualified Data.HashMap.Lazy as M
import           Data.Default

import           TrueSkill.Math
import           TrueSkill.Message

-- | The model parameters are gathered in this structure.
data Parameter d = Parameter
  { _skillSigma   :: !d
  , _drawMargin   :: !d
  }
makeLenses ''Parameter

type GameID = Int

data Player d = Player
  { _games :: M.HashMap GameID (Message d) -- ^ Includes all games that were
                                           --   used to infer the skill.
  , _skill :: Message d                    -- ^ Skill of this player.
  } deriving Show
makeLenses ''Player

instance Floating d => Default (Player d) where
  def = Player { _games = M.empty, _skill = def }

data Result = Won | Lost | Draw
  deriving (Show, Eq)

fuse3 f (a, a_) (b, b_) (c, c_) = (f a b c, f a_ b_ c_)
fuse2 f (a, a_) (b, b_) = (f a b, f a_ b_)

-- | Updates the skills of a set of players given a game.
update :: (Floating d, Ord d)
    => Parameter d -> GameID -> [Player d] -> [Player d] -> Result
    -> ([Player d], [Player d])
update parameter gameID playersLeft playersRight result =
    fuse3 update sentSkillPlayers treePassPlayers players
  where
    players = (playersLeft, playersRight)

    update :: Floating d => [Message d] -> [Message d] -> [Player d]
           -> [Player d]
    update = zipWith3 (\s m p -> games %~ (M.insert gameID m)
               $ skill .~ (s `include` m) $ p)

    treePassPlayers = treePass parameter sentSkillPlayers result

    sentSkillPlayers = (both %~ (map sentSkill) $ players)
    sentSkill :: Floating d => Player d -> Message d
    sentSkill player = view skill player
                       `exclude`
                       (M.lookupDefault def gameID $ view games player)

-- | Tansfers final prediction message into a result.
toResult :: (Floating d, Ord d) => Parameter d -> Message d -> Result
toResult parameter m
  | won > draw && won > lost  = Won
  | draw > won && draw > lost = Draw
  | lost > won && lost > draw = Lost
  where
    (lost, draw, won) = toResultProbabilities parameter m

-- | Tansfers final prediction message into a probabilistic result.
toResultProbabilities :: (Floating d, Ord d)
    => Parameter d -> Message d -> (d, d, d)
toResultProbabilities parameter m =
    (cdf (-eps), cdf eps - cdf (-eps), 1 - cdf eps)
  where
    eps = parameter^.drawMargin
    (mu, sigma2) = toMuSigma2 m
    sigma = sqrt sigma2

    cdf x = normCdf ((x - mu) / sigma)

-- | Calculates the Gaussian belief of a game result.
predict :: Floating d => Parameter d -> [Player d] -> [Player d]
        -> Message d
predict parameter playersLeft playersRight = toDifferenceMsg
  where
    players = (playersLeft, playersRight)

    toDifferenceMsg = toDifference performanceMsgs

    performanceMsgs = (both %~ fromPerformance $ skillMsgs)

    skillMsgs = both %~ (map (fromSkill $ parameter^.skillSigma)) $ msgs

    msgs = (both %~ (map (view skill)) $ players)

-- | A complete message pass down to the observed result variable and back.
treePass :: (Floating d, Ord d)
    => Parameter d -> ([Message d], [Message d]) -> Result
    -> ([Message d], [Message d])
treePass parameter msgs Lost = swap $ treePass parameter (swap msgs) Won
  where
    swap (a, b) = (b, a)
treePass parameter msgs result =
    both %~ (map (toSkill $ parameter^.skillSigma))
    $ fuse2 toPerformance skillMsgs fromDifferenceMsg
  where
    fromDifferenceMsg = fromDifference performanceMsgs
                          (marginal `exclude` toDifferenceMsg)

    marginal = case result of
        Won  -> differenceMarginalWon  (parameter^.drawMargin) toDifferenceMsg
        Draw -> differenceMarginalDraw (parameter^.drawMargin) toDifferenceMsg

    toDifferenceMsg = toDifference performanceMsgs

    performanceMsgs = (both %~ fromPerformance $ skillMsgs)

    skillMsgs = both %~ (map (fromSkill $ parameter^.skillSigma)) $ msgs

fromSkill :: Floating d => d -> Message d -> Message d
fromSkill beta msg = Message
    { _pi_  = a * msg^.pi_
    , _tau = a * msg^.tau
    }
  where
    a = 1.0 / (1.0 + c2 * msg^.pi_)
    c2 = beta ** 2

-- | Pass of a weighted sum.
weightedPass :: Floating d => [(d, Message d)] -> Message d
weightedPass msgs = Message
    { _pi_  = piNew
    , _tau = piNew * (sum $ map (\(a, m) -> a * m^.tau / m^.pi_) msgs)
    }
  where
    piNew = 1.0 / (sum $ map (\(a, m) -> a**2 / m^.pi_) msgs)

-- | Calculates the belief of the team skill given player skills.
fromPerformance :: Floating d => [Message d] -> Message d
fromPerformance msgs = weightedPass $ zip (repeat 1) msgs

toDifference :: Floating d => (Message d, Message d) -> Message d
toDifference (performanceLeftMsg, performanceRightMsg) =
    weightedPass [(1, performanceLeftMsg), (-1, performanceRightMsg)]

-- | EP messages from the observed game result variable given that the first
-- team won.
differenceMarginalWon :: (Floating d, Ord d) => d -> Message d -> Message d
differenceMarginalWon eps msg = differenceMarginal eps vWon wWon msg
  where
    wWon t eps_ = vWon_ * (vWon_ + t - eps_)
      where
        vWon_ = vWon t eps_
    vWon t eps_ = normPdf (t - eps_) / normCdf (t - eps_)

-- | EP messages from the observed game result variable given that the first
-- team won.
differenceMarginalDraw :: (Floating d, Ord d) => d -> Message d -> Message d
differenceMarginalDraw eps msg = differenceMarginal eps vDraw wDraw msg
  where
    wDraw t eps_ = vDraw_**2 +
        ((eps_ - t) * normPdf (eps_ - t) + (eps_ + t) * normPdf (eps_ + t)) /
        (normCdf (eps_ - t) - normCdf (-eps_ - t))
      where
        vDraw_ = vDraw t eps_

    vDraw t eps_ = (normPdf (-eps_ - t) - normPdf (eps_ - t)) /
        (normCdf (eps_ - t) - normCdf (-eps_ - t))

-- | Helper function for EP messages.
differenceMarginal :: Floating d => d ->
  (d -> d -> d) -> (d -> d -> d)
  -> Message d -> Message d
differenceMarginal eps vFun wFun msg = Message
    { _pi_  = c / wFun'
    , _tau = (d + sqrtC * vFun') / wFun'
    }
  where
    -- The notation follows the one in the original TrueSkill paper.
    wFun' = 1 - wFun (d / sqrtC) (eps * sqrtC)
    vFun' = vFun (d / sqrtC) (eps * sqrtC)


    c = msg^.pi_
    d = msg^.tau

    sqrtC = sqrt c

-- | Calculates the messages from the difference random variable back up to the
-- team variables.
fromDifference :: Floating d => (Message d, Message d) -> Message d
               -> (Message d, Message d)
fromDifference (performanceLeftMsg, performanceRightMsg) toDifferenceFactorMsg =
    ( weightedPass [(1, performanceRightMsg), (1, toDifferenceFactorMsg)]
    , weightedPass [(1, performanceLeftMsg), (-1, toDifferenceFactorMsg)]
    )

-- | Given the message from the difference random variable this function
-- calculates the messages to each of the involved player game skill variables
-- of a team.
toPerformance :: Floating d => [Message d] -> Message d -> [Message d]
toPerformance fromPerformanceMsgs msg = go [] fromPerformanceMsgs
  where
    go _ []                    = []
    go headMsgs (m : tailMsgs) = weightedPass ((1, msg)
                                 : (zipM1 headMsgs)
                                 ++ (zipM1 tailMsgs))
        : go (m : headMsgs) tailMsgs

    zipM1 msgs = zip (repeat (-1)) msgs

-- | Message for a player skill variable given its corresponding game variable.
toSkill :: Floating d => d -> Message d -> Message d
toSkill beta = fromSkill beta

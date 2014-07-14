{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : TrueSkill
Description : Implements TrueSkill, a team ranking inference algorithm.
Copyright   : (c) Martin Kiefel, 2014
License     : BSD2
Maintainer  : mk@nopw.de
Stability   : experimental
Portability : portable

See http://machinelearning.wustl.edu/mlpapers/paper_files/NIPS2006_688.pdf
-}
module TrueSkill where

import           Control.Lens
import           Data.Number.Erf ( normcdf )
import qualified Data.HashMap.Lazy as M

-- | Msg represents a parameteric message, which is sent between the random
-- variables in the factor graph.
--
-- Messages are parametrized by the sufficient statistics of a Gaussian
-- distribution.
data Msg = Msg
  { _pi_  :: !Double
  , _tau :: !Double
  }
makeLenses ''Msg

-- | Translates the sufficient statistics of a message to the more readable
-- standard parameters for a Gaussian distribution -- mean and variance.
toMuSigma2 :: Msg -> (Double, Double)
toMuSigma2 msg = (mu, sigma2)
  where
    sigma2 = 1.0 / msg^.pi_
    mu = msg^.tau * sigma2

instance Show Msg where
  show m = "Msg (" ++ show mu ++ ", " ++ show sigma2 ++ ")"
    where
      (mu, sigma2) = toMuSigma2 m

type GameID = Int

data Player = Player
  { _games :: M.HashMap Int Msg -- ^ Includes all games that were used to infer
                                -- the skill.
  , _skill :: Msg               -- ^ Skill of this player.
  } deriving Show
makeLenses ''Player

data Result = Won | Lost | Draw

-- | Default player skill mean.
defaultMu :: Double
defaultMu = 25.0

-- | Default player skill standard deviation.
defaultSigma :: Double
defaultSigma = (defaultMu / 5.0)

-- | Default player skill variance.
defaultSigma2 :: Double
defaultSigma2 = defaultSigma**2

defaultPlayer :: Player
defaultPlayer = Player
      { _skill = Msg (1.0 / sigma2) (mu / sigma2)
      , _games = M.empty
      }
  where
    mu = defaultMu
    sigma2 = defaultSigma2

-- | Include a message in a belief.
include :: Msg -> Msg -> Msg
include stateLeft stateRight =
    Msg
      { _pi_  = stateLeft^.pi_ + stateRight^.pi_
      , _tau = stateLeft^.tau + stateRight^.tau
      }

-- | Remove a message from a belief.
exclude :: Msg -> Msg -> Msg
exclude stateLeft stateRight =
    Msg
      { _pi_  = stateLeft^.pi_ - stateRight^.pi_
      , _tau = stateLeft^.tau - stateRight^.tau
      }

fuse3 f (a, a_) (b, b_) (c, c_) = (f a b c, f a_ b_ c_)
fuse2 f (a, a_) (b, b_) = (f a b, f a_ b_)

-- | Updates the skills of a set of players given a game.
update :: Int -> [Player] -> [Player] -> Result -> ([Player], [Player])
update gameID playersLeft playersRight result =
    fuse3 update sentSkillPlayers treePassPlayers players
  where
    players = (playersLeft, playersRight)

    update :: [Msg] -> [Msg] -> [Player] -> [Player]
    update = zipWith3 (\s m p -> games %~ (M.insert gameID m) $ skill .~ (s `include` m) $ p)

    treePassPlayers = treePass sentSkillPlayers result

    sentSkillPlayers = (both %~ (map sentSkill) $ players)
    sentSkill :: Player -> Msg
    sentSkill player = view skill player `exclude` (M.lookupDefault emptyMsg gameID $ view games player)

    emptyMsg = Msg { _pi_ = 0, _tau = 0 }

-- | Calculates the Gaussian belief of a game result.
predict :: [Player] -> [Player] -> Msg
predict playersLeft playersRight = toDifferenceMsg
  where
    players = (playersLeft, playersRight)

    toDifferenceMsg = toDifference performanceMsgs

    performanceMsgs = (both %~ fromPerformance $ skillMsgs)

    skillMsgs = both %~ (map fromSkill) $ msgs

    msgs = (both %~ (map (view skill)) $ players)

-- | A complete message pass down to the observed result variable and back.
treePass :: ([Msg], [Msg]) -> Result -> ([Msg], [Msg])
treePass msgs Lost = swap $ treePass (swap msgs) Won
  where
    swap (a, b) = (b, a)
treePass msgs result = both %~ (map toSkill) $
    fuse2 toPerformance skillMsgs fromDifferenceMsg
  where
    fromDifferenceMsg :: (Msg, Msg)
    fromDifferenceMsg = fromDifference performanceMsgs (marginal `exclude` toDifferenceMsg)

    marginal :: Msg
    marginal = case result of
        Won  -> differenceMarginalWon  toDifferenceMsg
        Draw -> differenceMarginalDraw toDifferenceMsg

    toDifferenceMsg = toDifference performanceMsgs

    performanceMsgs = (both %~ fromPerformance $ skillMsgs)

    skillMsgs = both %~ (map fromSkill) $ msgs

-- | Game skill likelihood variance.
beta :: Double
beta = (defaultSigma / 5.0)

fromSkill :: Msg -> Msg
fromSkill msg = Msg
    { _pi_  = a * msg^.pi_
    , _tau = a * msg^.tau
    }
  where
    a = 1.0 / (1.0 + c2 * msg^.pi_)
    c2 = beta ** 2

-- | Pass of a weighted sum.
weightedPass :: [(Double, Msg)] -> Msg
weightedPass msgs = Msg
    { _pi_  = piNew
    , _tau = piNew * (sum $ map (\(a, m) -> a * m^.tau / m^.pi_) msgs)
    }
  where
    piNew :: Double
    piNew = 1.0 / (sum $ map (\(a, m) -> a**2 / m^.pi_) msgs)

-- | Calculates the belief of the team skill given player skills.
fromPerformance :: [Msg] -> Msg
fromPerformance msgs = weightedPass $ zip (repeat 1) msgs


toDifference :: (Msg, Msg) -> Msg
toDifference (performanceLeftMsg, performanceRightMsg) =
    weightedPass [(1, performanceLeftMsg), (-1, performanceRightMsg)]

-- | Probability density function of a normal Gaussian distribution.
normpdf :: Double -> Double
normpdf x = exp (-x**2 / 2) / sqrt (2 * pi)

-- | EP messages from the observed game result variable given that the first
-- team won.
differenceMarginalWon :: Msg -> Msg
differenceMarginalWon msg = differenceMarginal vWon wWon msg
  where
    wWon t eps_ = vWon_ * (vWon_ + t - eps_)
      where
        vWon_ = vWon t eps_
    vWon t eps_ = normpdf (t - eps_) / normcdf (t - eps_)

-- | EP messages from the observed game result variable given that the first
-- team won.
differenceMarginalDraw :: Msg -> Msg
differenceMarginalDraw msg = differenceMarginal vDraw wDraw msg
  where
    wDraw t eps_ = vDraw_**2 +
        ((eps_ - t) * normpdf (eps_ - t) + (eps_ + t) * normpdf (eps_ + t)) /
        (normcdf (eps_ - t) - normcdf (-eps_ - t))
      where
        vDraw_ = vDraw t eps_

    vDraw t eps_ = (normpdf (-eps_ - t) - normpdf (eps_ - t)) /
        (normcdf (eps_ - t) - normcdf (-eps_ - t))

-- eps set by
-- 0.2166588675713617 = 2 * normcdf(eps / (sqrt 2 * ((25.0 / 3.0) / 2.0))) - 1
-- >> norminv(1.2166588675713617 / 2)
--
-- ans =
--
--     0.2750
-- | Margin in which a game is considered being a draw. See paper from above
-- for further explanation.
eps :: Double
eps = 0.2750 * (sqrt 2 * beta)

-- | Helper function for EP messages.
differenceMarginal ::
  (Double -> Double -> Double)
  -> (Double -> Double -> Double)
  -> Msg -> Msg
differenceMarginal vFun wFun msg = Msg
    { _pi_  = c / wFun_
    , _tau = (d + sqrtC * vFun_) / wFun_
    }
  where
    wFun_ = 1 - wFun (d / sqrtC) (eps * sqrtC)
    vFun_ = vFun (d / sqrtC) (eps * sqrtC)

    c = msg^.pi_
    d = msg^.tau

    sqrtC = sqrt c


-- | Calculates the messages from the difference random variable back up to the
-- team variables.
fromDifference :: (Msg, Msg) -> Msg -> (Msg, Msg)
fromDifference (performanceLeftMsg, performanceRightMsg) toDifferenceFactorMsg =
    ( weightedPass [(1, performanceRightMsg), (1, toDifferenceFactorMsg)]
    , weightedPass [(1, performanceLeftMsg), (-1, toDifferenceFactorMsg)]
    )

-- | Given the message from the difference random variable this function
-- calculates the messages to each of the involved player game skill variables
-- of a team.
toPerformance :: [Msg] -> Msg -> [Msg]
toPerformance fromPerformanceMsgs msg = go [] fromPerformanceMsgs
  where
    go _ []                    = []
    go headMsgs (m : tailMsgs) = weightedPass ((1, msg) : (zipM1 headMsgs) ++ (zipM1 tailMsgs))
        : go (m : headMsgs) tailMsgs

    zipM1 msgs = zip (repeat (-1)) msgs

-- | Message for a player skill variable given its corresponding game variable.
toSkill :: Msg -> Msg
toSkill = fromSkill

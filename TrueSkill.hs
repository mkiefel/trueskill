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
import qualified Data.HashMap.Lazy as M

-- | Msg represents a parameteric message, which is sent between the random
-- variables in the factor graph.
--
-- Messages are parametrized by the sufficient statistics of a Gaussian
-- distribution.
data Msg d = Msg
  { _pi_ :: !d
  , _tau :: !d
  }
makeLenses ''Msg

-- | Translates the sufficient statistics of a message to the more readable
-- standard parameters for a Gaussian distribution -- mean and variance.
toMuSigma2 :: Floating a => Msg a -> (a, a)
toMuSigma2 msg = (mu, sigma2)
  where
    sigma2 = 1.0 / msg^.pi_
    mu = msg^.tau * sigma2

instance (Show d, Floating d) => Show (Msg d) where
  show m = "Msg (" ++ show mu ++ ", " ++ show sigma2 ++ ")"
    where
      (mu, sigma2) = toMuSigma2 m

type GameID = Int

data Player d = Player
  { _games :: M.HashMap GameID (Msg d) -- ^ Includes all games that were used to infer
                                    -- the skill.
  , _skill :: Msg d                 -- ^ Skill of this player.
  } deriving Show
makeLenses ''Player

data Result = Won | Lost | Draw
  deriving (Show, Eq)

-- | Default player skill mean.
defaultMu :: Floating d => d
defaultMu = 25.0

-- | Default player skill standard deviation.
defaultSigma :: Floating d => d
defaultSigma = (defaultMu / 5.0)

-- | Default player skill variance.
defaultSigma2 :: Floating d => d
defaultSigma2 = defaultSigma**2

defaultPlayer :: Floating d => Player d
defaultPlayer = Player
      { _skill = Msg (1.0 / sigma2) (mu / sigma2)
      , _games = M.empty
      }
  where
    mu = defaultMu
    sigma2 = defaultSigma2

-- | Include a message in a belief.
include :: Floating d => Msg d -> Msg d -> Msg d
include stateLeft stateRight =
    Msg
      { _pi_  = stateLeft^.pi_ + stateRight^.pi_
      , _tau = stateLeft^.tau + stateRight^.tau
      }

-- | Remove a message from a belief.
exclude :: Floating d => Msg d -> Msg d -> Msg d
exclude stateLeft stateRight =
    Msg
      { _pi_  = stateLeft^.pi_ - stateRight^.pi_
      , _tau = stateLeft^.tau - stateRight^.tau
      }

emptyMsg :: Floating d => Msg d
emptyMsg = Msg { _pi_ = 0, _tau = 0 }

fuse3 f (a, a_) (b, b_) (c, c_) = (f a b c, f a_ b_ c_)
fuse2 f (a, a_) (b, b_) = (f a b, f a_ b_)

-- | Updates the skills of a set of players given a game.
update :: (Floating d, Ord d) => GameID -> [Player d] -> [Player d] -> Result -> ([Player d], [Player d])
update gameID playersLeft playersRight result =
    fuse3 update sentSkillPlayers treePassPlayers players
  where
    players = (playersLeft, playersRight)

    update :: Floating d => [Msg d] -> [Msg d] -> [Player d] -> [Player d]
    update = zipWith3 (\s m p -> games %~ (M.insert gameID m) $ skill .~ (s `include` m) $ p)

    treePassPlayers = treePass sentSkillPlayers result

    sentSkillPlayers = (both %~ (map sentSkill) $ players)
    sentSkill :: Floating d => Player d -> Msg d
    sentSkill player = view skill player `exclude` (M.lookupDefault emptyMsg gameID $ view games player)

-- | Tansfers final prediction message into a result.
toResult :: (Floating d, Ord d) => Msg d -> Result
toResult m
  | mu > eps  = Won
  | mu < -eps = Lost
  | otherwise = Draw
  where
    (mu, sigma2) = toMuSigma2 m

-- | Tansfers final prediction message into a probabilistic result.
toResultProbabilities :: (Floating d, Ord d) => Msg d -> (d, d, d)
toResultProbabilities m = (cdf (-eps), cdf eps - cdf (-eps), 1 - cdf eps)
  where
    (mu, sigma2) = toMuSigma2 m
    sigma = sqrt sigma2

    cdf x = normcdf ((x - mu) / sigma)

-- | Calculates the Gaussian belief of a game result.
predict :: Floating d => [Player d] -> [Player d] -> Msg d
predict playersLeft playersRight = toDifferenceMsg
  where
    players = (playersLeft, playersRight)

    toDifferenceMsg = toDifference performanceMsgs

    performanceMsgs = (both %~ fromPerformance $ skillMsgs)

    skillMsgs = both %~ (map fromSkill) $ msgs

    msgs = (both %~ (map (view skill)) $ players)

-- | A complete message pass down to the observed result variable and back.
treePass :: (Floating d, Ord d) => ([Msg d], [Msg d]) -> Result -> ([Msg d], [Msg d])
treePass msgs Lost = swap $ treePass (swap msgs) Won
  where
    swap (a, b) = (b, a)
treePass msgs result = both %~ (map toSkill) $
    fuse2 toPerformance skillMsgs fromDifferenceMsg
  where
    {-fromDifferenceMsg :: (Msg d, Msg d)-}
    fromDifferenceMsg = fromDifference performanceMsgs (marginal `exclude` toDifferenceMsg)

    {-marginal :: Msg d-}
    marginal = case result of
        Won  -> differenceMarginalWon  toDifferenceMsg
        Draw -> differenceMarginalDraw toDifferenceMsg

    toDifferenceMsg = toDifference performanceMsgs

    performanceMsgs = (both %~ fromPerformance $ skillMsgs)

    skillMsgs = both %~ (map fromSkill) $ msgs

-- | Game skill likelihood variance.
beta :: Floating d => d
beta = (defaultSigma / 5.0)

fromSkill :: Floating d => Msg d -> Msg d
fromSkill msg = Msg
    { _pi_  = a * msg^.pi_
    , _tau = a * msg^.tau
    }
  where
    a = 1.0 / (1.0 + c2 * msg^.pi_)
    c2 = beta ** 2

-- | Pass of a weighted sum.
weightedPass :: Floating d => [(d, Msg d)] -> Msg d
weightedPass msgs = Msg
    { _pi_  = piNew
    , _tau = piNew * (sum $ map (\(a, m) -> a * m^.tau / m^.pi_) msgs)
    }
  where
    piNew = 1.0 / (sum $ map (\(a, m) -> a**2 / m^.pi_) msgs)

-- | Calculates the belief of the team skill given player skills.
fromPerformance :: Floating d => [Msg d] -> Msg d
fromPerformance msgs = weightedPass $ zip (repeat 1) msgs

toDifference :: Floating d => (Msg d, Msg d) -> Msg d
toDifference (performanceLeftMsg, performanceRightMsg) =
    weightedPass [(1, performanceLeftMsg), (-1, performanceRightMsg)]

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

-- | EP messages from the observed game result variable given that the first
-- team won.
differenceMarginalWon :: (Floating d, Ord d) => Msg d -> Msg d
differenceMarginalWon msg = differenceMarginal vWon wWon msg
  where
    wWon t eps_ = vWon_ * (vWon_ + t - eps_)
      where
        vWon_ = vWon t eps_
    vWon t eps_ = normpdf (t - eps_) / normcdf (t - eps_)

-- | EP messages from the observed game result variable given that the first
-- team won.
differenceMarginalDraw :: (Floating d, Ord d) => Msg d -> Msg d
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
eps :: Floating d => d
eps = 0.2750 * (sqrt 2 * beta)

-- | Helper function for EP messages.
differenceMarginal :: Floating d =>
  (d -> d -> d) -> (d -> d -> d)
  -> Msg d -> Msg d
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
fromDifference :: Floating d => (Msg d, Msg d) -> Msg d -> (Msg d, Msg d)
fromDifference (performanceLeftMsg, performanceRightMsg) toDifferenceFactorMsg =
    ( weightedPass [(1, performanceRightMsg), (1, toDifferenceFactorMsg)]
    , weightedPass [(1, performanceLeftMsg), (-1, toDifferenceFactorMsg)]
    )

-- | Given the message from the difference random variable this function
-- calculates the messages to each of the involved player game skill variables
-- of a team.
toPerformance :: Floating d => [Msg d] -> Msg d -> [Msg d]
toPerformance fromPerformanceMsgs msg = go [] fromPerformanceMsgs
  where
    go _ []                    = []
    go headMsgs (m : tailMsgs) = weightedPass ((1, msg) : (zipM1 headMsgs) ++ (zipM1 tailMsgs))
        : go (m : headMsgs) tailMsgs

    zipM1 msgs = zip (repeat (-1)) msgs

-- | Message for a player skill variable given its corresponding game variable.
toSkill :: Floating d => Msg d -> Msg d
toSkill = fromSkill

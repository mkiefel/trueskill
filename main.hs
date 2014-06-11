{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Lens
import           Data.Number.Erf

data Msg = Msg
  { _pi_  :: !Double
  , _tau :: !Double
  }
makeLenses ''Msg

instance Show Msg where
  show m = "Msg (" ++ show mu ++ ", " ++ show sigma2 ++ ")"
    where
      sigma2 = 1.0 / m^.pi_
      mu = m^.tau * sigma2

data Player = Player
  { _identifier :: !String
  , _skill      :: !Msg
  } deriving Show
makeLenses ''Player

data Result = Won | Lost | Draw

a :: Player
a = Player
      { _identifier = "player 1"
      , _skill      = Msg (1.0 / sigma2) (mu / sigma2)
      }
  where
    mu = 25.0
    sigma2 = (25.0 / 3.0)**2

m :: Msg
m = Msg 0 0

include :: Msg -> Msg -> Msg
include stateLeft stateRight =
    Msg
      { _pi_  = stateLeft^.pi_ + stateRight^.pi_
      , _tau = stateLeft^.tau + stateRight^.tau
      }

exclude :: Msg -> Msg -> Msg
exclude stateLeft stateRight =
    Msg
      { _pi_  = stateLeft^.pi_ - stateRight^.pi_
      , _tau = stateLeft^.tau - stateRight^.tau
      }

update :: Player -> Player -> Result -> (Player, Player)
update playerLeft playerRight result =
    ( update (fst treePassPlayers) playerLeft
    , update (snd treePassPlayers) playerRight
    )
  where
    players = (playerLeft, playerRight)

    update :: Msg -> Player -> Player
    update msg player = skill %~ (`include` msg) $ player

    treePassPlayers = treePass (both %~ (view skill) $ players) result

treePass :: (Msg, Msg) -> Result -> (Msg, Msg)
treePass msgs Lost = swap $ treePass (swap msgs) Won
  where
    swap (a, b) = (b, a)
treePass msgs result = both %~ (toSkill . toPerformance) $ fromDifferenceMsg
  where
    fromDifferenceMsg :: (Msg, Msg)
    fromDifferenceMsg = fromDifference performanceMsgs (marginal `exclude` toDifferenceMsg)

    marginal :: Msg
    marginal = case result of
        Won  -> differenceMarginalWon  toDifferenceMsg
        Draw -> differenceMarginalDraw toDifferenceMsg

    toDifferenceMsg = toDifference performanceMsgs

    performanceMsgs =
      (both %~ (fromPerformance . fromSkill) $ msgs)


fromSkill :: Msg -> Msg
fromSkill msg = Msg
    { _pi_  = a * msg^.pi_
    , _tau = a * msg^.tau
    }
  where
    a = 1.0 / (1.0 + c2 * msg^.pi_)
    c2 = 1

weightedPass :: [(Double, Msg)] -> Msg
weightedPass msgs = Msg
    { _pi_  = piNew
    , _tau = piNew * (sum $ map (\(a, m) -> a * m^.tau / m^.pi_) msgs)
    }
  where
    piNew :: Double
    piNew = 1.0 / (sum $ map (\(a, m) -> a**2 / m^.pi_) msgs)

fromPerformance :: Msg -> Msg
fromPerformance msg = weightedPass [(1, msg)]

toDifference :: (Msg, Msg) -> Msg
toDifference (performanceLeftMsg, performanceRightMsg) =
    weightedPass [(1, performanceLeftMsg), (-1, performanceRightMsg)]

normpdf x = exp (-x**2 / 2) / sqrt (2 * pi)

differenceMarginalWon :: Msg -> Msg
differenceMarginalWon msg = differenceMarginal vWon wWon msg
  where
    wWon t eps = vWon_ * (vWon_ + t - eps)
      where
        vWon_ = vWon t eps
    vWon t eps = normpdf (t - eps) / normcdf (t - eps)

differenceMarginalDraw :: Msg -> Msg
differenceMarginalDraw msg = differenceMarginal vDraw wDraw msg
  where
    wDraw t eps = vDraw_**2 +
        ((eps - t) * normpdf (eps - t) + (eps + t) * normpdf (eps + t)) /
        (normcdf (eps - t) - normcdf (-eps - t))
      where
        vDraw_ = vDraw t eps

    vDraw t eps = (normpdf (-eps - t) - normpdf (eps - t)) /
        (normcdf (eps - t) - normcdf (-eps - t))

differenceMarginal :: (Double -> Double -> Double) -> (Double -> Double -> Double) -> Msg -> Msg
differenceMarginal vFun wFun msg = Msg
    { _pi_  = c / wFun_
    , _tau = (d + sqrtC * vFun_) / wFun_
    }
  where
    eps = 0.25

    wFun_ = 1 - wFun (d / sqrtC) (eps * sqrtC)
    vFun_ = vFun (d / sqrtC) (eps * sqrtC)

    c = msg^.pi_
    d = msg^.tau

    sqrtC = sqrt c

fromDifference :: (Msg, Msg) -> Msg -> (Msg, Msg)
fromDifference (performanceLeftMsg, performanceRightMsg) toDifferenceFactorMsg =
    ( weightedPass [(1, performanceRightMsg), (1, toDifferenceFactorMsg)]
    , weightedPass [(1, performanceLeftMsg), (-1, toDifferenceFactorMsg)]
    )

toPerformance :: Msg -> Msg
toPerformance = fromPerformance

toSkill :: Msg -> Msg
toSkill = fromSkill

main = print $ update a a Won

{-# LANGUAGE TemplateHaskell,OverloadedStrings #-}
module Main where

import           Control.Lens
import           Data.Number.Erf
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as M
import           Data.Csv
import qualified Data.Vector as V
import           Data.Vector ( (!) )
import qualified Data.ByteString.Lazy as BL

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
  { _skill      :: !Msg
  } deriving Show
makeLenses ''Player

data Result = Won | Lost | Draw

defaultPlayer :: Player
defaultPlayer = Player
      { _skill      = Msg (1.0 / sigma2) (mu / sigma2)
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

fuse f (a, b) (c, d) = (f a c, f b d)

update :: [Player] -> [Player] -> Result -> ([Player], [Player])
update playersLeft playersRight result = fuse update treePassPlayers players
  where
    players = (playersLeft, playersRight)

    update :: [Msg] -> [Player] -> [Player]
    update = zipWith (\m p -> skill %~ (`include` m) $ p)

    treePassPlayers = treePass (both %~ (map (view skill)) $ players) result

treePass :: ([Msg], [Msg]) -> Result -> ([Msg], [Msg])
treePass msgs Lost = swap $ treePass (swap msgs) Won
  where
    swap (a, b) = (b, a)
treePass msgs result = both %~ (map toSkill) $
    fuse toPerformance skillMsgs fromDifferenceMsg
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


fromSkill :: Msg -> Msg
fromSkill msg = Msg
    { _pi_  = a * msg^.pi_
    , _tau = a * msg^.tau
    }
  where
    a = 1.0 / (1.0 + c2 * msg^.pi_)
    c2 = ((25.0 / 3.0) / 2.0) ** 2

weightedPass :: [(Double, Msg)] -> Msg
weightedPass msgs = Msg
    { _pi_  = piNew
    , _tau = piNew * (sum $ map (\(a, m) -> a * m^.tau / m^.pi_) msgs)
    }
  where
    piNew :: Double
    piNew = 1.0 / (sum $ map (\(a, m) -> a**2 / m^.pi_) msgs)

fromPerformance :: [Msg] -> Msg
fromPerformance msgs = weightedPass $ zip (repeat 1) msgs

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

differenceMarginal ::
  (Double -> Double -> Double)
  -> (Double -> Double -> Double)
  -> Msg -> Msg
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

toPerformance :: [Msg] -> Msg -> [Msg]
toPerformance fromPerformanceMsgs msg = go [] fromPerformanceMsgs
  where
    go _ []                    = []
    go headMsgs (m : tailMsgs) = weightedPass ((1, msg) : (zipM1 headMsgs) ++ (zipM1 tailMsgs))
        : go (m : headMsgs) tailMsgs

    zipM1 msgs = zip (repeat (-1)) msgs

toSkill :: Msg -> Msg
toSkill = fromSkill

mangleRow :: M.HashMap String Player -> V.Vector String -> M.HashMap String Player
mangleRow players row = M.insert player2Name player2 $ M.insert player1Name player1 players
  where
    player1Name = row!1
    player2Name = row!2

    ([player1], [player2]) = update (get player1Name) (get player2Name) result

    result
      | ((score $ row!3) > (score $ row!4))  = Won
      | ((score $ row!3) == (score $ row!4)) = Draw
      | otherwise                            = Lost

    score :: String -> Int
    score s = read s

    get :: String -> [Player]
    get p = [M.lookupDefault defaultPlayer p players]

main = do
    csvData <- BL.getContents
    case decode NoHeader csvData of
        Left err -> putStrLn err
        Right v -> print $ V.foldl' mangleRow M.empty v

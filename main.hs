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
import           System.Environment ( getArgs )
import           System.Random.MWC
import           System.Random.MWC.Distributions
import           Data.List ( sortBy )
import           System.IO

data Msg = Msg
  { _pi_  :: !Double
  , _tau :: !Double
  }
makeLenses ''Msg

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
  { _games :: M.HashMap Int Msg
  , _skill :: Msg
  } deriving Show
makeLenses ''Player

data Result = Won | Lost | Draw

defaultMu = 25.0
defaultSigma = (defaultMu / 5.0)
defaultSigma2 = defaultSigma**2

defaultPlayer :: Player
defaultPlayer = Player
      { _skill = Msg (1.0 / sigma2) (mu / sigma2)
      , _games = M.empty
      }
  where
    mu = defaultMu
    sigma2 = defaultSigma2

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

fuse3 f (a, a_) (b, b_) (c, c_) = (f a b c, f a_ b_ c_)
fuse2 f (a, a_) (b, b_) = (f a b, f a_ b_)

update :: Int -> [Player] -> [Player] -> Result -> ([Player], [Player])
update gameID playersLeft playersRight result = fuse3 update sentSkillPlayers treePassPlayers players
  where
    players = (playersLeft, playersRight)

    update :: [Msg] -> [Msg] -> [Player] -> [Player]
    update = zipWith3 (\s m p -> games %~ (M.insert gameID m) $ skill .~ (s `include` m) $ p)

    treePassPlayers = treePass sentSkillPlayers result

    sentSkillPlayers = (both %~ (map sentSkill) $ players)
    sentSkill :: Player -> Msg
    sentSkill player = view skill player `exclude` (M.lookupDefault emptyMsg gameID $ view games player)

    emptyMsg = Msg { _pi_ = 0, _tau = 0 }

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

beta = (defaultSigma / 5.0)

fromSkill :: Msg -> Msg
fromSkill msg = Msg
    { _pi_  = a * msg^.pi_
    , _tau = a * msg^.tau
    }
  where
    a = 1.0 / (1.0 + c2 * msg^.pi_)
    c2 = beta ** 2

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
    wWon t eps_ = vWon_ * (vWon_ + t - eps_)
      where
        vWon_ = vWon t eps_
    vWon t eps_ = normpdf (t - eps_) / normcdf (t - eps_)

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
eps = 0.2750 * (sqrt 2 * beta)

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
    gameID = read $ row!5

    ([player1], [player2]) = update gameID (get player1Name) (get player2Name) result

    result
      | ((score $ row!3) > (score $ row!4))  = Won
      | ((score $ row!3) == (score $ row!4)) = Draw
      | otherwise                            = Lost

    score :: String -> Int
    score s = read s

    get :: String -> [Player]
    get p = [M.lookupDefault defaultPlayer p players]

countDraws v = V.length $ V.filter (\row -> row!3 == row!4) v

buildGoalTable :: M.HashMap String Player -> V.Vector (V.Vector String) -> [(Double, (Int, Int))]
buildGoalTable model v = V.toList $ V.filter checkProper
    $ V.map (\row -> entry (row!1) (row!2) (row!3) (row!4)) v
  where
    checkProper (mu, (score1, score2)) = mu > eps && score1 > score2 ||
      abs mu < eps && score1 == score2 ||
      mu < -eps && score1 < score2
    entry :: String -> String -> String -> String -> (Double, (Int, Int))
    entry player1Name player2Name score1 score2 = (mu, (read score1, read score2))
      where
        (mu, sigma2) = toMuSigma2 toDifferenceMsg

        toDifferenceMsg = toDifference performanceMsgs

        performanceMsgs = (both %~ fromPerformance $ skillMsgs)

        skillMsgs = both %~ (map fromSkill) $ ([view skill $ get player1Name], [view skill $ get player2Name])

        get p = M.lookupDefault defaultPlayer p model

queryRow :: M.HashMap String Player -> [(Double, (Int, Int))] -> V.Vector String -> (Int, String, String, Int, Int, Double, Double)
queryRow model table row = (eval, player1Name, player2Name, fst $ snd best, snd $ snd best, mu, sigma2)
  where
    player1Name = row!1
    player2Name = row!2

    best = bestScore mu

    bestScore sample = foldl keepBetter (head table) table
      where
        keepBetter t1@(d, s) t2@(d_, s_)
          | abs (d_ - sample) < abs (d - sample) = t2
          | otherwise                            = t1

    (mu, sigma2) = toMuSigma2 toDifferenceMsg

    toDifferenceMsg = toDifference performanceMsgs

    performanceMsgs = (both %~ fromPerformance $ skillMsgs)

    skillMsgs = both %~ (map fromSkill) $ ([view skill $ get player1Name], [view skill $ get player2Name])

    get p = M.lookupDefault undefined p model

    -- evaluation
    eval
        | V.length row < 4 = 0
        | score1 == score1_ && score2 == score2_ = 4
        | score1 /= score2 && (score1 - score2) == (score1_ - score2_) = 3
        | score1 == score2 && (score1 - score2) == (score1_ - score2_) = 2
        | (score1 > score2 && score1_ > score2_) || (score1 < score2 && score1_ < score2_) = 2
        | otherwise = 0
      where
        score1 = read (row!3)
        score2 = read (row!4)

        score1_ = fst $ snd best
        score2_ = snd $ snd best

findBestPlayer name player p@(name_, value_)
  | value_ < value = (name, value)
  | otherwise      = p
  where
    (mu, sigma2) = toMuSigma2 $ (view skill player)
    value = mu - 3 * sqrt sigma2

main = do
    [trainFile,goalFile] <- getArgs
    csvData <- BL.readFile trainFile
    case decode NoHeader csvData of
      Left err -> putStrLn err
      Right v -> do
        {-print ((fromIntegral $ countDraws v) / (fromIntegral $ V.length v))-}
        let model = V.foldl' mangleRow M.empty v
        hPutStrLn stderr $ show model

        let (best, value) = M.foldrWithKey findBestPlayer ("noland", -100) model
        putStrLn $ best ++ ": " ++ show value

        csvData <- BL.readFile goalFile
        case decode NoHeader csvData of
          Left err -> putStrLn err
          Right g -> do
            let goalTable = buildGoalTable model g
            {-hPutStrLn stderr $ show goalTable-}

            queryData <- BL.getContents
            let results = case decode NoHeader queryData of
                            Left err -> V.empty
                            Right q -> V.map (queryRow model goalTable) q

            BL.putStr $ encode $ V.toList results


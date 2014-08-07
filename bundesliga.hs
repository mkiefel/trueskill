{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Main where

import qualified Data.HashMap.Lazy as M
import           Data.Csv ( HasHeader(..), decode, encode )
import qualified Data.Vector as V
import           Data.Vector ( (!) )
import qualified Data.ByteString.Lazy as BL
import           System.Environment ( getArgs )
import           System.IO ( stderr, hPutStrLn )
import           Control.Lens
import           Data.List ( foldl' )
import           Control.Monad.Trans.Either ( EitherT(..)
                                            , runEitherT
                                            , hoistEither )
import           Control.Monad.Trans.Class ( lift )

import           Numeric.AD.Mode.Forward
import           Linear
import           Optimization.LineSearch
import           Optimization.LineSearch.BFGS

import           Debug.Trace

import           TrueSkill ( predict
                           , update
                           , toMuSigma2
                           , toResult
                           , toResultProbabilities
                           , skill
                           , Parameter(..)
                           , skillSigma
                           , drawMargin
                           , Msg(..)
                           , Player(..)
                           , Result(..) )

type Model d = M.HashMap String (Player d)

data Game = Game
  { _team1  :: ![String]
  , _team2  :: ![String]
  , _result :: !Result
  , _gameID :: !Int
  }
makeLenses ''Game

-- | Default player skill mean.
defaultMu :: Floating d => d
defaultMu = 25.0

-- | Default player skill standard deviation.
defaultSigma :: Floating d => d
defaultSigma = (defaultMu / 5.0)

-- | Default player skill variance.
defaultSigma2 :: Floating d => d
defaultSigma2 = defaultSigma**2

{-defaultPlayer :: Floating d => Player d-}
{-defaultPlayer = Player-}
      {-{ _skill = Msg (1.0 / sigma2) (mu / sigma2)-}
      {-, _games = M.empty-}
      {-}-}
  {-where-}
    {-mu = defaultMu-}
    {-sigma2 = defaultSigma2-}

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

-- | Game skill likelihood variance.
beta :: Floating d => d
beta = (defaultSigma / 5.0)

defaultParameter :: Floating d => Parameter d
defaultParameter = Parameter
    { _skillSigma = beta
    , _drawMargin = eps
    }

{-defaultParameter :: Floating d => Parameter d-}
{-defaultParameter = Parameter-}
    {-{ _skillSigma = 5.052192801828535-}
    {-, _drawMargin = 5.094179195149669-}
    {-}-}

-- | Transforms a CSV row into a game.
parseGame :: V.Vector String -> Game
parseGame row = Game team1 team2 result gameID
  where
    team1 = map (\i -> row!i) [7..7+10]
    team2 = map (\i -> row!i) [18..18+10]
    gameID = read $ row!0

    [score1String, score2String] = splitBy ':' $ head $ splitBy ' ' $ row!6
    score = (read $ score1String, read $ score2String) :: (Int, Int)

    result
      | ((fst score)  > (snd score)) = Won
      | ((fst score) == (snd score)) = Draw
      | otherwise                    = Lost

    splitBy delimiter = foldr f [[]]
      where
        f c l@(x:xs) | c == delimiter = []:l
                     | otherwise = (c:x):xs

updateModel :: (Floating d, Ord d)
    => Parameter d -> Player d -> Model d -> Game -> Model d
updateModel parameter defaultPlayer players game = updatedModel
  where
    (updatedTeam1, updatedTeam2) = update parameter
                                    (game ^. gameID)
                                    (map get $ game ^. team1)
                                    (map get $ game ^. team2)
                                    (game ^. result)

    updatedModel = foldl' put players
                    $ zip (game ^. team1 ++ game ^. team2)
                          (updatedTeam1 ++ updatedTeam2)

    {-put :: Model -> (String, Player Double) -> Model-}
    put m (p, player) = M.insert p player m

    {-get :: String -> Player Double-}
    get p = M.lookupDefault defaultPlayer p players

findBestPlayer name player p@(name_, value_)
  | value_ < value = (name, value)
  | otherwise      = p
  where
    (mu, sigma2) = toMuSigma2 $ (view skill player)
    value = mu - 3 * sqrt sigma2

trainModel :: (Floating d, Ord d)
    => Parameter d -> Player d -> V.Vector Game -> Model d
trainModel parameter defaultPlayer games =
    V.foldl' (updateModel parameter defaultPlayer) M.empty games

testModel :: (Floating d, Ord d)
    => Parameter d -> Player d -> Model d -> V.Vector Game
    -> V.Vector Result
testModel parameter defaultPlayer players games =
    V.map (\g -> toResult parameter
                 $ predict parameter (map get $ g ^. team1)
                                     (map get $ g ^. team2)) games
  where
    {-get :: String -> Player Double-}
    get p = M.lookupDefault defaultPlayer p players

testModelProbability :: (Floating d, Ord d)
    => Parameter d -> Player d -> Model d -> V.Vector Game -> V.Vector (d, d, d)
testModelProbability parameter defaultPlayer players games =
    V.map (\g -> toResultProbabilities parameter
                 $ predict defaultParameter (map get $ g ^. team1)
                                            (map get $ g ^. team2)) games
  where
    {-get :: String -> Player Double-}
    get p = M.lookupDefault defaultPlayer p players


objective trainData valData [skillSigma, drawMargin, playerSigma] =
    V.sum $ V.map readout $ V.zip prediction valData
  where
    parameter = Parameter
      { _skillSigma = skillSigma
      , _drawMargin = drawMargin
      }

    player = Player
      { _skill = Msg (1 / playerSigma2) (defaultMu / playerSigma2)
      , _games = M.empty
      }

    playerSigma2 = playerSigma ** 2

    model = trainModel parameter player trainData
    prediction = testModelProbability parameter player model valData

    readout ((lost, draw, won), game) =
        case (game ^. result) of
          Won  -> -log won
          Draw -> -log draw
          Lost -> -log lost

instance Metric []

optimizer f df init = bfgs search df [[1, 0, 0], [0, 1, 0], [0, 0, 1]] init
  where
    search = armijoSearch 0.1 0.2 0.2 wrappedF

    wrappedF x = Debug.Trace.trace (show x ++ ": " ++ show v) v
      where
        v = f x

main = do
    [trainFile, valFile, testFile] <- getArgs
    trainFileData <- BL.readFile trainFile
    valFileData <- BL.readFile valFile
    testFileData <- BL.readFile testFile

    results <- runEitherT $ do
        trainSingleData <- decodeCsv trainFileData
        valData   <- decodeCsv valFileData

        -- for loopy belief propagation repeat the schedule
        let trainData = trainSingleData V.++ trainSingleData V.++ trainSingleData


        let ps = take 20 $ optimizer (objective trainData valData)
                    (grad $ objective trainData valData)
                    [2.4610671085195572,2.7530053800029552,4.875459753758152]
                    [>[ defaultParameter^.skillSigma<]
                    [>, defaultParameter^.drawMargin<]
                    [>, defaultSigma<]
                    [>] :: [[Double]]<]
        lift $ print ps

        let [skillSigma, drawMargin, playerSigma] = last ps
        {-let [skillSigma, drawMargin, playerSigma] = [4.618792693710958,3.9896633441188776,2.1175572048484756]-}

        let playerSigma2 = playerSigma ** 2
        let trainedParameter = Parameter
                                { _skillSigma = skillSigma
                                , _drawMargin = drawMargin }
        let trainedPlayer = Player
                              { _skill = Msg (1 / playerSigma2)
                                         (defaultMu / playerSigma2)
                              , _games = M.empty
                              }

        {-let trainedParameter = defaultParameter-}

        testData <- decodeCsv testFileData
        let model = trainModel trainedParameter trainedPlayer trainData
                      :: Model Double

        let (best, value) = M.foldrWithKey
                              findBestPlayer ("noland", -100) model
        lift $ putStrLn $ best ++ ": " ++ show value

        let prediction = testModel trainedParameter trainedPlayer model
                           testData

        return $ V.length
               $ V.filter (\(g, p) -> (g ^. result) == p)
               $ V.zip testData prediction
    report results

  where
    report (Left err)      = putStrLn err
    report (Right results) = print results

    decodeCsv :: Monad m => BL.ByteString -> EitherT String m (V.Vector Game)
    decodeCsv = (V.map parseGame `fmap`) . hoistEither . decode NoHeader

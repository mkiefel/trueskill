{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Main where

import qualified Data.HashMap.Strict as M
import           Data.Csv ( HasHeader(..), decode, encode )
import           Data.Default ( def )
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
                           , train
                           , toMuSigma2
                           , fromMuSigma2
                           , skills
                           , makeSkills
                           , Parameter(..)
                           , sigmaOffense
                           , sigmaDefense
                           , predictionMessage
                           , Player
                           , Result(..) )

type Model d = M.HashMap String (Player d)

data Game = Game
  { _team1  :: ![String]
  , _team2  :: ![String]
  , _result :: !Result
  , _gameID :: !Int
  } deriving Show
makeLenses ''Game

-- | Default player skill mean.
defaultMuOffense :: Floating d => d
defaultMuOffense = 3.0 / 11.0

-- | Default player skill standard deviation.
defaultSigmaOffense :: Floating d => d
defaultSigmaOffense = 0.1

-- | Default player skill mean.
defaultMuDefense :: Floating d => d
defaultMuDefense = 1.0 / 11.0

-- | Default player skill standard deviation.
defaultSigmaDefense :: Floating d => d
defaultSigmaDefense = 0.1

defaultParameter :: Floating d => Parameter d
defaultParameter = Parameter
    { _sigmaOffense = defaultSigmaOffense / 5.0
    , _sigmaDefense = defaultSigmaDefense / 5.0
    }

-- | Transforms a CSV row into a game.
parseGame :: V.Vector String -> Game
parseGame row = Game team1 team2 result gameID
  where
    team1 = map (\i -> row!i) [7..7+10]
    team2 = map (\i -> row!i) [18..18+10]
    gameID = read $ row!0

    [score1String, score2String] = splitBy ':' $ head $ splitBy ' ' $ row!6
    result = Result (read score1String, read score2String)

    splitBy delimiter = foldr f [[]]
      where
        f c l@(x:xs) | c == delimiter = []:l
                     | otherwise = (c:x):xs

updateModel :: (Floating d, Ord d)
    => Parameter d -> Player d -> Model d -> Game -> Model d
updateModel parameter defaultPlayer players game = updatedModel
  where
    (updatedTeam1, updatedTeam2) = train parameter
                                    (game ^. gameID)
                                    (game ^. result)
                                    ( (map get $ game ^. team1)
                                    , (map get $ game ^. team2) )

    updatedModel = foldl' put players
                    $ zip (game ^. team1 ++ game ^. team2)
                          (updatedTeam1 ++ updatedTeam2)

    put m (p, player) = M.insert p player m

    get p = M.lookupDefault defaultPlayer p players

{-findBestPlayer name player p@(name_, value_)-}
  {-| value_ < value = (name, value)-}
  {-| otherwise      = p-}
  {-where-}
    {-(mu, sigma2) = toMuSigma2 $ view skill player-}
    {-value = mu - 3 * sqrt sigma2-}

trainModel :: (Floating d, Ord d)
    => Parameter d -> Player d -> V.Vector Game -> Model d
trainModel parameter defaultPlayer =
    V.foldl' (updateModel parameter defaultPlayer) M.empty

{-testModel :: (Floating d, Ord d)-}
    {-=> Parameter d -> Player d -> Model d -> V.Vector Game-}
    {--> V.Vector Result-}
{-testModel parameter defaultPlayer players games =-}
    {-V.map (\g -> toResult parameter-}
                 {-$ predict parameter (map get $ g ^. team1)-}
                                     {-(map get $ g ^. team2)) games-}
  {-where-}
    {-get p = M.lookupDefault defaultPlayer p players-}

{-testModelProbability :: (Floating d, Ord d)-}
    {-=> Parameter d -> Player d -> Model d -> V.Vector Game -> V.Vector (d, d, d)-}
{-testModelProbability parameter defaultPlayer players games =-}
    {-V.map (\g -> toResultProbabilities parameter-}
                 {-$ predict defaultParameter (map get $ g ^. team1)-}
                                            {-(map get $ g ^. team2)) games-}
  {-where-}
    {-get p = M.lookupDefault defaultPlayer p players-}


objective trainData valData [ sigmaOffense
                            , sigmaDefense
                            , muOffense
                            , sigmaOffense2
                            , muDefense
                            , sigmaDefense2] = -- model
    V.sum $ V.map loss valData
  where
    parameter = Parameter
      { _sigmaOffense = sigmaOffense
      , _sigmaDefense = sigmaDefense
      }

    defaultPlayer = skills .~ makeSkills
                    (fromMuSigma2 muOffense sigmaOffense2)
                    (fromMuSigma2 muDefense sigmaDefense2)
                    $ def

    model = trainModel parameter defaultPlayer trainData

    loss game = uncurry (readout (game ^. result))
                $ both %~ predictionMessage
                $ predict parameter ( map get $ game ^. team1
                                    , map get $ game ^. team2
                                    )
    readout (Result (g1, g2)) p1 p2 = -log (p1 !! g1) - log (p2 !! g2)
      where
        -- msg = "(" ++ show g1 ++ ", " ++ show g2 ++ "): " ++ show p1 ++ ", " ++ show p2
        -- msg = "."

    get p = M.lookupDefault defaultPlayer p model

optimizer f df = bfgs search df [ [1, 0, 0, 0, 0, 0]
                                , [0, 1, 0, 0, 0, 0]
                                , [0, 0, 1, 0, 0, 0]
                                , [0, 0, 0, 1, 0, 0]
                                , [0, 0, 0, 0, 1, 0]
                                , [0, 0, 0, 0, 0, 1]
                                ]
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
        valData         <- decodeCsv valFileData

        -- for loopy belief propagation repeat the schedule
        let trainData = trainSingleData

        let ps = take 20 $ optimizer (objective trainData valData)
                    (grad $ objective trainData valData)
                    [ defaultParameter ^. sigmaOffense
                    , defaultParameter ^. sigmaDefense
                    , defaultMuOffense
                    , defaultSigmaOffense**2
                    , defaultMuDefense
                    , defaultSigmaDefense**2
                    ]

        lift $ print ps

        let [ sigmaOffense'
              , sigmaDefense'
              , muOffense'
              , sigmaOffense2'
              , muDefense'
              , sigmaDefense2' ] = last ps

        let trainedParameter = Parameter
                                { _sigmaOffense = sigmaOffense'
                                , _sigmaDefense = sigmaDefense' }

        let trainedPlayer = skills .~ makeSkills
                            (fromMuSigma2 muOffense' sigmaOffense2')
                            (fromMuSigma2 muDefense' sigmaDefense2')
                            $ def

        testData <- decodeCsv testFileData
        let model = trainModel trainedParameter trainedPlayer trainData
                    :: Model Double

        {-let (best, value) = M.foldrWithKey-}
                              {-findBestPlayer ("noland", -100) model-}
        {-lift $ putStrLn $ best ++ ": " ++ show value-}

        {-let prediction = testModel trainedParameter trainedPlayer model-}
                           {-testData-}

        {-return $ V.length-}
               {-$ V.filter (\(g, p) -> (g ^. result) == p)-}
               {-$ V.zip testData prediction-}
        return 0
    report results

  where
    report (Left err)      = putStrLn err
    report (Right results) = print results

    decodeCsv :: Monad m => BL.ByteString -> EitherT String m (V.Vector Game)
    decodeCsv = (V.map parseGame `fmap`) . hoistEither . decode NoHeader

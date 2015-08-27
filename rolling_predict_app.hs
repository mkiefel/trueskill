{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Lens hiding ( Context, contexts )
import           Control.Monad.Trans.Either ( EitherT(..)
                                            , runEitherT
                                            , hoistEither )
import           Control.Monad.Trans.Class ( lift )
import           Control.Applicative ( (<*>), (<$>) )
import           Data.Default ( def )
import qualified Data.HashMap.Strict as M
import           Data.List ( foldl'
                           , intercalate
                           , sortBy )
import qualified Data.Vector as V
import           Data.Csv ( encode
                          , ToRecord(..)
                          , record
                          , toField )
import qualified Data.ByteString.Lazy as BL
import           System.Environment ( getArgs )
import           Text.Printf ( printf )

import           TrueSkill ( predict
                           , fromMuSigma2
                           , toMuSigma2
                           , skills
                           , games
                           , makeSkills
                           , Parameter(..)
                           , Message
                           , predictionMessage
                           , offense
                           , defense
                           , Result(..) )

import           Train
import           Types
import           Parameter

data Prediction = Prediction
                  { _predictionHome         :: Int
                  , _predictionGuest        :: Int
                  , _predictionExpectedLoss :: Double
                  }
makeLenses ''Prediction

predictionSpace :: [Prediction]
predictionSpace = Prediction <$> [0..9] <*> [0..9] <*> [0]

loss :: Game -> Result -> Prediction -> Double
loss game (Result (home, guest)) p
  | home == guest && p^.predictionHome == p^.predictionGuest = -game^.gameOdds._2
  | home >  guest && p^.predictionHome >  p^.predictionGuest = -game^.gameOdds._1
  | home <  guest && p^.predictionHome <  p^.predictionGuest = -game^.gameOdds._3
  | otherwise                                                = 0
  -- fromIntegral ((home - p^.predictionHome)^(2 :: Int)) +
  -- fromIntegral ((guest - p^.predictionGuest)^(2 :: Int))

data Context = Context
               { _contextPrediction    :: Prediction
               , _contextProbabilities :: ([Double], [Double])
               , _contextMessage       :: (Message Double, Message Double)
               , _contextGame          :: Game
               , _contextModel         :: Model Double
               }
makeLenses ''Context

data LatentPlayer = LatentPlayer
                    { playerName         :: !String
                    , playerMuOffense    :: !Double
                    , playerSigmaOffense :: !Double
                    , playerMuDefense    :: !Double
                    , playerSigmaDefense :: !Double
                    , playerOffenseScore :: !Double
                    , playerDefenseScore :: !Double
                    , playerGames        :: Int
                    }
instance ToRecord LatentPlayer where
    toRecord p = record $ map (\f -> f p)
                 [ toField . playerName
                 , toField . playerMuOffense
                 , toField . playerSigmaOffense
                 , toField . playerMuDefense
                 , toField . playerSigmaDefense
                 , toField . playerOffenseScore
                 , toField . playerDefenseScore ]

instance Show Prediction where
  show p = printf "%d:%d - %f" (p^.predictionHome) (p^.predictionGuest) (p^.predictionExpectedLoss)

instance Show Context where
  show c = printf "(%d, %d):" homeGoals guestGoals ++
           " -> " ++ (show $ c^.contextPrediction)
    where
      Result (homeGoals, guestGoals) = c^.contextGame.result

instance ToRecord Context where
  toRecord c = record $ (toField $ c^.contextGame.gameID) :

               (toField $ show homeGoals ++ ":" ++ show guestGoals) :
               (toField $ show $ c^.contextPrediction) :

               [toField muPredictionHomeMessage,
                toField sigma2PredictionHomeMessage] ++
               [toField muPredictionGuestMessage,
                toField sigma2PredictionGuestMessage] ++

               map toField (c^.contextProbabilities._1) ++
               map toField (c^.contextProbabilities._2)
    where
      Result (homeGoals, guestGoals) = c^.contextGame.result

      (muPredictionHomeMessage, sigma2PredictionHomeMessage) =
        toMuSigma2 $ c^.contextMessage._1
      (muPredictionGuestMessage, sigma2PredictionGuestMessage) =
        toMuSigma2 $ c^.contextMessage._2

argMin :: Ord d => [(d, a)] -> (d, a)
argMin []     = undefined
argMin (s:ss) = foldl' go s ss
    where
    go left@(v, _) right@(w, _)
        | w < v     = right
        | otherwise = left

decide :: Game -> ([Double], [Double]) -> Prediction
decide game probabilities = prediction { _predictionExpectedLoss = minLoss }
  where
    (minLoss, prediction) = argMin predictionCosts

    predictionCosts :: [(Double, Prediction)]
    predictionCosts = map (\p -> (predictionCost p, p)) predictionSpace

    predictionCost :: Prediction -> Double
    predictionCost p = sum $ map (\ ((h, hp), (g, gp)) ->
                       hp * gp * loss game (Result (h, g)) p) $
                       ((,) <$>
                       (zip [0..] (fst probabilities)) <*>
                       (zip [0..] (snd probabilities)))

rollingPredict :: FilePath -> FilePath -> Knobs -> IO (Either String Double)
rollingPredict trainFile testFile knobs = runEitherT $ do
    trainData <- hoistEither =<<
                 lift (readGamesFromCsv trainFile)
    testData <- hoistEither =<<
                lift (readGamesFromCsv testFile)

    let initModel = trainModel (getMessagePasses knobs) parameter
                    defaultPlayer trainData
    let initContext = Context undefined undefined undefined undefined initModel
    let contexts = V.scanl' roll initContext testData

    let finalPrediction = V.last contexts

    let latentPlayers = map convert $ M.toList $
                        finalPrediction^.contextModel

    lift $ BL.writeFile "player.csv" $ encode $ latentPlayers
    lift $ BL.writeFile "games.csv" $ encode $
      V.toList $ V.tail contexts

    lift $ putStrLn "Offense:"
    lift $ putStrLn $ intercalate "\n" $ map playerName $ take 12
      $ sortLadder playerOffenseScore latentPlayers
    lift $ putStrLn ""

    lift $ putStrLn "Defense:"
    lift $ putStrLn $ intercalate "\n" $ map playerName $ take 12
      $ sortLadder playerDefenseScore latentPlayers
    lift $ putStrLn ""


    return $ (V.sum $ V.map
      (\c -> (loss (c^.contextGame) (c^.contextGame.result) (c^.contextPrediction) + 1)) $
      V.filter (\c -> c^.contextPrediction.predictionExpectedLoss + 1 < 0) $
      V.tail contexts)
  where
    sortLadder crit players =
      reverse
      $ sortBy (\p p' ->
                  compare (crit p) (crit p'))
      $ filter (\p -> playerGames p >= 30)
      players

    evalPlayer skill' p = mu - 2 * sqrt sigma2
      where
        (mu, sigma2) = toMuSigma2 (p^.skills.skill')

    convert (name, p) = LatentPlayer name
                        muOffense sigmaOffense
                        muDefense sigmaDefense
                        (evalPlayer offense p)
                        (evalPlayer defense p)
                        (length $ M.toList $ p^.games)
      where
        (muOffense, sigmaOffense) = toMuSigma2 (p^.skills.offense)
        (muDefense, sigmaDefense) = toMuSigma2 (p^.skills.defense)


    roll context game =
        Context prediction probabilities
        (fromHomeMessage, fromGuestMessage) game $
        model
        -- updateModel parameter defaultPlayer model game
      where
        model = context ^. contextModel

        get p = M.lookupDefault defaultPlayer p model

        (fromHomeMessage, fromGuestMessage) =
          predict parameter
          ( map get $ game ^. team1
          , map get $ game ^. team2
          )

        probabilities = both %~ predictionMessage $
                        (fromHomeMessage, fromGuestMessage)
        prediction = decide game probabilities

    parameter =
      (def :: Parameter Double)
      { _sigmaOffense = getSigmaOffense knobs
      , _sigmaDefense = getSigmaDefense knobs
      , _homeBonus = makeSkills
                     (fromMuSigma2 (getMuHomeBonusOffense knobs)
                      (getSigmaHomeBonusOffense knobs ^ (2 :: Int)))
                     (fromMuSigma2 (getMuHomeBonusDefense knobs)
                      (getSigmaHomeBonusDefense knobs ^ (2 :: Int)))
      }

    defaultPlayer = skills .~ makeSkills
                    (fromMuSigma2 (getDefaultMuOffense knobs)
                     (getDefaultSigmaOffense knobs ^ (2 :: Int)))
                    (fromMuSigma2 (getDefaultMuDefense knobs)
                     (getDefaultSigmaDefense knobs ^ (2 :: Int)))
                    $ def

main :: IO ()
main = do
  [trainFile, testFile, knobsFile] <- getArgs

  rawKnobs <- readKnobs knobsFile

  case rawKnobs of
    Just knobs ->
      print =<< rollingPredict trainFile testFile knobs
    Nothing ->
      putStrLn "error reading knobs file"

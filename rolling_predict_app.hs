{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Lens
import           Control.Monad.Trans.Either ( EitherT(..)
                                            , runEitherT
                                            , hoistEither )
import           Control.Monad.Trans.Class ( lift )
import           Data.Default ( def )
import qualified Data.HashMap.Strict as M
import qualified Data.Vector as V
-- import           Data.List ( sortBy )
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
                           , makeSkills
                           , Parameter(..)
                           , predictionMessage
                           , offense
                           , defense
                           , Result(..) )
import           TrueSkill.Math ( argMax )

import           Train
import           Types
import           Parameter

data Prediction = Prediction
                  { _predictionHome  :: [Double]
                  , _predictionGuest :: [Double]
                  , _predictionGame  :: Game
                  , _predictionModel :: Model Double
                  }
makeLenses ''Prediction


data PlayerOut = PlayerOut { playerName :: !String
                           , playerMuOffense :: !Double
                           , playerSigmaOffense :: !Double
                           , playerMuDefense :: !Double
                           , playerSigmaDefense :: !Double
                           , playerOffenseScore :: !Double
                           , playerDefenseScore :: !Double }
instance ToRecord PlayerOut where
    toRecord p = record $ map (\f -> f p)
                 [ toField . playerName
                 , toField . playerMuOffense
                 , toField . playerSigmaOffense
                 , toField . playerMuDefense
                 , toField . playerSigmaDefense
                 , toField . playerOffenseScore
                 , toField . playerDefenseScore ]


instance Show Prediction where
  show p = printf "(%d, %d):\n" homeGoals guestGoals ++
           concatMap (printf "%0.2f, ") (p^.predictionHome) ++ "\n" ++
           concatMap (printf "%0.2f, ") (p^.predictionGuest) ++ "\n" ++
           printf "-> (%d, %d): (%0.2f)\n" predHomeGoals predGuestGoals
           (loss p)
    where
      Result (homeGoals, guestGoals) = p^.predictionGame.result

      (predHomeGoals, predGuestGoals) = score p

instance ToRecord Prediction where
  toRecord p = record $ (toField $ p^.predictionGame.gameID) :
               (toField $ show homeGoals ++ ":" ++ show guestGoals) :
               (toField $ show predHomeGoals ++ ":" ++ show predHomeGoals) :
               map toField (p^.predictionHome) ++
               map toField (p^.predictionGuest)
    where
      Result (homeGoals, guestGoals) = p^.predictionGame.result
      (predHomeGoals, predGuestGoals) = score p

loss :: Prediction -> Double
loss p = error' predHomeGoals homeGoals
         + error' predGuestGoals guestGoals
  where
    Result (homeGoals, guestGoals) = p^.predictionGame.result

    (predHomeGoals, predGuestGoals) = score p

    error' :: Int -> Int -> Double
    error' predGoals goals = fromIntegral ((predGoals - goals)^(2 :: Int))

score :: Prediction -> (Int, Int)
score p = ( bestPrediction (p^.predictionHome)
          , bestPrediction (p^.predictionGuest) )
  where

    bestPrediction :: [Double] -> Int
    bestPrediction = argMax . map (\d -> -d) . predictionCosts

    predictionCosts :: [Double] -> [Double]
    predictionCosts probs = map (predictionCost probs) $ [0..length probs]

    predictionCost :: [Double] -> Int -> Double
    predictionCost probs g = sum $ map
                             (\(g', prob) -> prob
                                             * fromIntegral ((g' - g)^(2 :: Int)))
                             $ zip [0..] probs


rollingPredict :: FilePath -> FilePath -> Knobs -> IO (Either String ())
rollingPredict trainFile testFile knobs = runEitherT $ do
    trainData <- hoistEither =<<
                 lift (readGamesFromCsv trainFile)
    testData <- hoistEither =<<
                lift (readGamesFromCsv testFile)

    let initModel = trainModel (getMessagePasses knobs) parameter
                    defaultPlayer trainData
    let initPrediction = Prediction undefined undefined undefined initModel
    let predictions = V.scanl' roll initPrediction testData

    -- lift $ mapM_ print $ V.toList $ V.tail predictions

    let mse = (V.sum $ V.map loss $ V.tail predictions)
              / fromIntegral ((V.length $ V.tail predictions) * 2)
    lift $ print mse

    let finalPrediction = V.last predictions

    -- Print the best offense/defense player.
    -- lift $ print $ sortPlayer (evalPlayer offense) $
    --   finalPrediction^.predictionModel
    -- lift $ print $ sortPlayer (evalPlayer defense) $
    --   finalPrediction^.predictionModel
    lift $ BL.writeFile "player.csv" $ encode $
      map convert $ M.toList $
      finalPrediction^.predictionModel
    lift $ BL.writeFile "games.csv" $ encode $
      V.toList $ V.tail predictions
  where
    evalPlayer skill' p = mu - 2 * sqrt sigma2
      where
        (mu, sigma2) = toMuSigma2 (p^.skills.skill')

    -- sortPlayer l m = sortBy (\(_, s) (_, s') -> compare s s') $
    --                  mapped._2 %~ l $
    --                  M.toList m

    convert (name, p) = PlayerOut name
                        muOffense sigmaOffense
                        muDefense sigmaDefense
                        (evalPlayer offense p)
                        (evalPlayer defense p)
      where
        (muOffense, sigmaOffense) = toMuSigma2 (p^.skills.offense)
        (muDefense, sigmaDefense) = toMuSigma2 (p^.skills.offense)


    roll prediction game =
        Prediction homeGoals guestGoals game $ model
        -- updateModel parameter defaultPlayer model game
      where
        model = prediction ^. predictionModel

        get p = M.lookupDefault defaultPlayer p model

        (homeGoals, guestGoals) = both %~ predictionMessage $
                                  predict parameter
                                  ( map get $ game ^. team1
                                  , map get $ game ^. team2
                                  )

    parameter =
      def
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

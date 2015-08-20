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

instance Show Prediction where
  show p = printf "(%d, %d):\n" homeGoals guestGoals ++
           concatMap (printf "%0.2f, ") (p^.predictionHome) ++ "\n" ++
           concatMap (printf "%0.2f, ") (p^.predictionGuest) ++ "\n" ++
           printf "-> (%d, %d): (%0.2f)\n" predHomeGoals predGuestGoals
           (loss p)
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

    lift $ mapM_ print $ V.toList $ V.tail predictions

    let mse = (V.sum $ V.map loss $ V.tail predictions)
              / fromIntegral ((V.length $ V.tail predictions) * 2)
    lift $ print mse

    let finalPrediction = V.last predictions

    -- Print the best offense/defense player.
    lift $ print $ findMaxPlayer (evalPlayer offense) 0 $
      finalPrediction^.predictionModel
    lift $ print $ findMaxPlayer (evalPlayer defense) 0 $
      finalPrediction^.predictionModel
  where
    evalPlayer skill' p = mu - 2 * sqrt sigma2
      where
        (mu, sigma2) = toMuSigma2 (p^.skills.skill')

    findMaxPlayer fun i = M.foldlWithKey' go ("n/a", i)
      where
        go c@(_, v) name' p'
          | v' > v    = (name', v')
          | otherwise = c
            where
              v' = fun p'

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
                      (getSigmaHomeBonusOffense2 knobs))
                     (fromMuSigma2 (getMuHomeBonusDefense knobs)
                      (getSigmaHomeBonusDefense2 knobs))
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

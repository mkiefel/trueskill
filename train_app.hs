module Main where

import           System.Environment ( getArgs )
import           Control.Lens
import           Control.Monad.Trans.Either ( EitherT(..)
                                            , runEitherT
                                            , hoistEither )
import           Control.Monad.Trans.Class ( lift )
import           Control.Parallel.Strategies
import qualified Data.Vector as V
import           Linear.V2

import           TrueSkill.Autodiff hiding ( lift )

import           Parameter
import           Types
import           Train

import           Debug.Trace

parallelObjectiveGrad :: Int -> V.Vector Game -> V.Vector Game
                      -> [Double] -> [Double]
parallelObjectiveGrad passes trainData valData parameter = runEval $ do
  let offsets = [ 0, 2, 4 ]

  let adParameter = map liftParameter offsets
  values <- mapM (rpar . objective passes trainData valData) adParameter
  mapM_ rseq values

  let gradient = concatMap (readGradient . getGradient) values

  return $ trace ("snorm: " ++ show (snorm gradient)) gradient
    where
      snorm gradient = sum $ map (^ (2 :: Int)) gradient

      readGradient v = [ v^._x, v^._y ]
      liftParameter offset = map go $ zip [0..] parameter
        where
          go (i, p) = makeAD p (i - offset)

train :: FilePath -> FilePath -> FilePath -> Knobs
      -> IO (Either String ())
train trainFile valFile outKnobsFile knobs = runEitherT $ do
  trainData <- hoistEither =<<
               (lift $ readGamesFromCsv trainFile)
  valData <- hoistEither =<<
             (lift $ readGamesFromCsv valFile)

  let ps = take 200 $ optimizer
           (objective (getMessagePasses knobs) trainData valData)
           (parallelObjectiveGrad (getMessagePasses knobs) trainData
            valData)
           [ getSigmaOffense knobs
           , getSigmaDefense knobs
           , getDefaultMuOffense knobs
           , getDefaultSigmaOffense knobs ^ (2 :: Int)
           , getDefaultMuDefense knobs
           , getDefaultSigmaDefense knobs ^ (2 :: Int)
           ]

  lift $ print ps

  let [ sigmaOffense'
        , sigmaDefense'
        , defaultMuOffense'
        , defaultSigmaOffense2'
        , defaultMuDefense'
        , defaultSigmaDefense2' ] = last ps

  lift $ writeKnobs outKnobsFile $
    Knobs defaultMuOffense' (sqrt defaultSigmaOffense2')
    defaultMuDefense' (sqrt defaultSigmaDefense2')
    sigmaOffense' sigmaDefense' (getMessagePasses knobs)

main :: IO ()
main = do
  [trainFile, valFile, inKnobsFile, outKnobsFile] <- getArgs

  rawKnobs <- readKnobs inKnobsFile

  case rawKnobs of
    Just knobs ->
      train trainFile valFile outKnobsFile knobs >>= report
    Nothing ->
      putStrLn "error reading knobs file"

  where
    report (Left err)      = putStrLn err
    report (Right results) = print results

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
import qualified TrueSkill.Autodiff as AD

import           Parameter
import           Types
import           Train

import           Debug.Trace

scale :: Double
scale = 10.0

parallelObjectiveGrad :: Int -> V.Vector Game -> V.Vector Game
                      -> [Double] -> [Double]
parallelObjectiveGrad passes trainData valData parameter = runEval $ do
  let offsets = [ 0, 2, 4, 6, 8 ]

  let adParameter = map liftParameter offsets
  values <- mapM (rpar . objective passes trainData valData) adParameter
  mapM_ rseq values

  let gradient = concatMap (readGradient . getGradient) values
  let (v:_) = map getValue values

  let clean_gradient = if any isNaN gradient
                       then replicate (length gradient) 10000
                       else gradient

  return $ trace (show parameter ++ ": " ++ show v ++ "; "
                  ++ show clean_gradient) clean_gradient
    where
      readGradient v = [ v^._x, v^._y ]
      liftParameter offset = map go $ zip [0..] parameter
        where
          go (i, p) = (makeAD p (i - offset)) / AD.lift scale

train :: FilePath -> FilePath -> FilePath -> Knobs
      -> IO (Either String ())
train trainFile valFile outKnobsFile knobs = runEitherT $ do
  trainData <- hoistEither =<<
               (lift $ readGamesFromCsv trainFile)
  valData <- hoistEither =<<
             (lift $ readGamesFromCsv valFile)

  let ps = map (/ scale) $ optimizer
           (objective (getMessagePasses knobs) trainData valData . map (/ scale))
           (parallelObjectiveGrad (getMessagePasses knobs) trainData
            valData) $ map (* scale)
           [ getSigmaOffense knobs
           , getSigmaDefense knobs
           , getDefaultMuOffense knobs
           , getDefaultSigmaOffense knobs ^ (2 :: Int)
           , getDefaultMuDefense knobs
           , getDefaultSigmaDefense knobs ^ (2 :: Int)
           , getMuHomeBonusOffense knobs
           , getSigmaHomeBonusOffense knobs ^ (2 :: Int)
           , getMuHomeBonusDefense knobs
           , getSigmaHomeBonusDefense knobs ^ (2 :: Int)
           ]

  lift $ print ps

  let [ sigmaOffense'
        , sigmaDefense'
        , defaultMuOffense'
        , defaultSigmaOffense2'
        , defaultMuDefense'
        , defaultSigmaDefense2'
        , muHomeBonusOffense'
        , sigmaHomeBonusOffense2'
        , muHomeBonusDefense'
        , sigmaHomeBonusDefense2'
        ] = ps

  lift $ writeKnobs outKnobsFile $
    Knobs defaultMuOffense' (sqrt defaultSigmaOffense2')
    defaultMuDefense' (sqrt defaultSigmaDefense2')
    sigmaOffense' sigmaDefense' (getMessagePasses knobs)
    muHomeBonusOffense' (sqrt sigmaHomeBonusOffense2')
    muHomeBonusDefense' (sqrt sigmaHomeBonusDefense2')

main :: IO ()
main = do
  [trainFile, valFile, inKnobsFile, outKnobsFile] <- getArgs

  rawKnobs <- readKnobs inKnobsFile
  print rawKnobs

  case rawKnobs of
    Just knobs ->
      train trainFile valFile outKnobsFile knobs >>= report
    Nothing ->
      putStrLn "error reading knobs file"

  where
    report (Left err)      = putStrLn err
    report (Right results) = print results

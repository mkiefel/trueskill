module Main where

import           System.Environment ( getArgs )
import           Control.Lens
import           Control.Monad.Trans.Either ( EitherT(..)
                                            , runEitherT
                                            , hoistEither )
import           Control.Monad.Trans.Class ( lift )
import           Control.Parallel.Strategies
import           Data.Default ( def )
import           Linear.V2

import           TrueSkill ( Parameter(..)
                           , Player
                           , skills
                           , makeSkills
                           , fromMuSigma2
                           , sigmaOffense
                           , sigmaDefense
                           )
import           TrueSkill.Autodiff hiding ( lift )

import           Types
import           Train

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

defaultPlayer :: Floating d => Player d
defaultPlayer = skills .~ makeSkills
                (fromMuSigma2 defaultMuOffense (defaultSigmaOffense**2))
                (fromMuSigma2 defaultMuDefense (defaultSigmaDefense**2))
                $ def

parallelObjectiveGrad trainData valData parameter = runEval $ do
  let offsets = [ 0, 2, 4 ]

  let adParameter = map (liftParameter parameter) offsets
  values <- mapM (rpar . objective trainData valData) adParameter
  mapM_ rseq values

  return $ concatMap (readGradient . getGradient) values
    where
      readGradient v = [ v^._x, v^._y ]
      liftParameter parameter offset = map go $ zip [0..] parameter
        where
          go (i, p) = makeAD p (i - offset)

main = do
    [trainFile, valFile, testFile] <- getArgs

    results <- runEitherT $ do
      trainData <- hoistEither =<<
                   (lift $ readGamesFromCsv trainFile)
      valData <- hoistEither =<<
                 (lift $ readGamesFromCsv valFile)

      let ps = take 20 $ optimizer (objective trainData valData)
               (parallelObjectiveGrad trainData valData)
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

      testData <- hoistEither =<<
                  (lift $ readGamesFromCsv trainFile)

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

module Main where

import           Criterion.Main
import           Control.Lens
import           Control.Monad.Trans.Class ( lift )
import           Control.Monad.Trans.Either ( EitherT(..)
                                            , runEitherT
                                            , hoistEither )
import           Data.Default ( def )
import qualified Data.Vector as V
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

import           Train
import           Types

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

buildEval :: V.Vector Game -> V.Vector Game -> Double
buildEval trainData valData = value
  where
    value = objective trainData valData [ defaultParameter ^. sigmaOffense
                                        , defaultParameter ^. sigmaDefense
                                        , defaultMuOffense
                                        , defaultSigmaOffense**2
                                        , defaultMuDefense
                                        , defaultSigmaDefense**2
                                        ]

gradEval :: V.Vector Game -> V.Vector Game -> (Double, V2 Double)
gradEval trainData valData = value `seq` grad `seq` (value, grad)
  where
    -- For testing with the `ad' package on hackage.

    -- (value, grad) = grad' objective' [ defaultParameter ^. sigmaOffense
    --                                  , defaultParameter ^. sigmaDefense
    --                                  ]

    -- objective' :: (Floating d, Ord d) => [d] -> d
    -- objective' [a, b] = objective trainData valData [ a
    --                                                 , b
    --                                                 , defaultMuOffense
    --                                                 , defaultSigmaOffense^2
    --                                                 , defaultMuDefense
    --                                                 , defaultSigmaDefense^2
    --                                                 ]

    AD value grad = objective trainData valData initial

    initial :: [AD]
    initial = map (uncurry makeAD) $ zip [ defaultParameter ^. sigmaOffense
                                         , defaultParameter ^. sigmaDefense
                                         , defaultMuOffense
                                         , defaultSigmaOffense^(2 :: Int)
                                         , defaultMuDefense
                                         , defaultSigmaDefense^(2 :: Int)
                                         ] [0..]

benchmark :: V.Vector Game -> V.Vector Game -> IO ()
benchmark trainData valData = defaultMain [
  bgroup "TrueSkill" [ bench "grad" $ whnf (gradEval trainData) valData
                     , bench "eval" $ whnf (buildEval trainData) valData
                     ]
  ]

main :: IO ()
main = do
  r <- runEitherT $ do
    trainData <- hoistEither =<<
                 (lift $
                  readGamesFromCsv "bundesliga/shuffled_train.csv")
    valData <- hoistEither =<<
               (lift $
                readGamesFromCsv "bundesliga/shuffled_validation.csv")

    lift $ benchmark trainData valData
    -- lift $ print $ gradEval trainData valData

  case r of
    Left err -> putStrLn err
    Right _  -> return ()

module Train where


import           Control.Lens
import           Data.Default ( def )
import qualified Data.HashMap.Strict as M
import           Data.List ( foldl' )
import qualified Data.Vector as V
import           Optimization.LineSearch
import           Optimization.LineSearch.BFGS

import           TrueSkill ( predict
                           , train
                           , fromMuSigma2
                           , skills
                           , makeSkills
                           , Parameter(..)
                           , predictionMessage
                           , Player
                           , Result(..) )

import           Types

import           Debug.Trace

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

trainModel :: (Floating d, Ord d)
    => Parameter d -> Player d -> V.Vector Game -> Model d
trainModel parameter defaultPlayer =
    V.foldl' (updateModel parameter defaultPlayer) M.empty

objective :: (Floating d, Ord d) => V.Vector Game -> V.Vector Game
          -> [d] -> d
objective trainData valData [ sigmaOffense
                            , sigmaDefense
                            , muOffense
                            , sigmaOffense2
                            , muDefense
                            , sigmaDefense2
                            ] =
    V.sum (V.map loss valData) / fromIntegral (V.length valData)
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

    get p = M.lookupDefault defaultPlayer p model
objective _ _ _ = undefined

optimizer :: ([Double] -> Double)
          -> ([Double] -> [Double])
          -> [Double]
          -> [[Double]]
optimizer f df = bfgs search df [ [1, 0, 0, 0, 0, 0]
                                , [0, 1, 0, 0, 0, 0]
                                , [0, 0, 1, 0, 0, 0]
                                , [0, 0, 0, 1, 0, 0]
                                , [0, 0, 0, 0, 1, 0]
                                , [0, 0, 0, 0, 0, 1]
                                ]
  where
    search = armijoSearch 0.1 0.2 0.2 wrappedF

    wrappedF x = trace (show x ++ ": " ++ show v) v
      where
        v = f x

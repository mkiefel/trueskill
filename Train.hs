module Train where

import           Control.Lens
import           Data.Default ( def )
import qualified Data.HashMap.Strict as M
import           Data.List ( foldl' )
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import           Data.Number.Erf

import           Numeric.LBFGSB
import           Numeric.LBFGSB.Result ( solution )

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

updateModel :: (Floating d, Ord d, Show d, Erf d)
    => Parameter d -> Player d -> Model d -> Game -> Model d
updateModel parameter defaultPlayer players game = updatedModel
  where
    (updatedTeam1, updatedTeam2) = train parameter
                                    (game ^. gameID)
                                    (game ^. result)
                                    ( map get $ game ^. team1
                                    , map get $ game ^. team2 )

    updatedModel = foldl' put players
                    $ zip (game ^. team1 ++ game ^. team2)
                          (updatedTeam1 ++ updatedTeam2)

    -- put m (p, player) = M.insert p player m
    put m (p, player)
      -- | p == "Lahm" = trace (show $ player^.skills) $ M.insert p player m
      | otherwise = M.insert p player m

    get p = M.lookupDefault defaultPlayer p players

trainModel :: (Floating d, Ord d, Show d, Erf d)
    => Int -> Parameter d -> Player d -> V.Vector Game -> Model d
trainModel passes parameter defaultPlayer games =
    foldl' singlePass M.empty $ replicate passes games
  where
    singlePass = V.foldl' (updateModel parameter defaultPlayer)

objective :: (Floating d, Ord d, Show d, Erf d) =>
             Int -> V.Vector Game -> V.Vector Game -> [d] -> d
objective passes trainData valData [ sigmaOffense
                                   , sigmaDefense
                                   , muOffense
                                   , sigmaOffense2
                                   , muDefense
                                   , sigmaDefense2
                                   , muHomeBonusOffense
                                   , sigmaHomeBonusOffense2
                                   , muHomeBonusDefense
                                   , sigmaHomeBonusDefense2
                                   ] =
    V.sum (V.map loss valData) / fromIntegral (V.length valData)
  where
    parameter = Parameter
      { _sigmaOffense = exp $ sigmaOffense
      , _sigmaDefense = exp $ sigmaDefense
      , _homeBonus = makeSkills
                     (fromMuSigma2 muHomeBonusOffense $ exp sigmaHomeBonusOffense2)
                     (fromMuSigma2 muHomeBonusDefense $ exp sigmaHomeBonusDefense2)
      }

    defaultPlayer = skills .~ makeSkills
                    (fromMuSigma2 muOffense $ exp sigmaOffense2)
                    (fromMuSigma2 muDefense $ exp sigmaDefense2)
                    $ def

    model = trainModel passes parameter defaultPlayer trainData

    loss game = uncurry (readout (game ^. result))
                $ both %~ predictionMessage
                $ predict parameter ( map get $ game ^. team1
                                    , map get $ game ^. team2
                                    )
    readout (Result (g1, g2)) p1 p2 = -log (p1 !! g1 + 1e-3) - log (p2 !! g2 + 1e-3)

    get p = M.lookupDefault defaultPlayer p model
-- The case when not the right number of arguments has been passed.
objective _ _ _ _ = undefined

optimizer :: ([Double] -> Double)
          -> ([Double] -> [Double])
          -> [Double]
          -> [Double]
optimizer f g initX =
  VS.toList
  $ solution
  $ minimize 5 1e7 1e-5 (Just steps) (repeat (Nothing, Nothing))
  (VS.fromList initX) wrappedF (VS.fromList . g . VS.toList)
  where
    steps = 100

    wrappedF x = trace (show x' ++ ": " ++ show fx') fx'
      where
        x' = VS.toList x
        fx = f x'
        fx'
          | isNaN fx = 100 -- 1 / 0
          | otherwise = fx

{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.HashMap.Lazy as M
import           Data.Csv ( HasHeader(..), decode, encode )
import qualified Data.Vector as V
import           Data.Vector ( (!) )
import qualified Data.ByteString.Lazy as BL
import           System.Environment ( getArgs )
import           System.IO ( stderr, hPutStrLn )
import           Control.Lens ( view )
import           Control.Monad.Trans.Either ( runEitherT, hoistEither )
import           Control.Monad.Trans.Class ( lift )

import           TrueSkill ( predict
                           , update
                           , eps
                           , defaultPlayer
                           , toMuSigma2
                           , skill
                           , Player
                           , Result(..) )

type Model = M.HashMap String (Player Double)
type ScoreLookup = [(Double, (Int, Int))]


-- | Take a game (which is in the passed row) and update the beliefs of the skill.
--
-- Games might be passed several times, this indirectly implements loopy belief
-- propagation. It is simply important that the reoccurring games always are
-- indexed with the same ID.
updateModel :: Model -> V.Vector String -> Model
updateModel players row = M.insert player2Name player2 $ M.insert player1Name player1 players
  where
    player1Name = row!1
    player2Name = row!2
    gameID = read $ row!5 :: Int
    score = (read $ row!3, read $ row!4) :: (Int, Int)

    ([player1], [player2]) = update gameID (get player1Name) (get player2Name) result

    result
      | ((fst score)  > (snd score)) = Won
      | ((fst score) == (snd score)) = Draw
      | otherwise                    = Lost


    get :: String -> [Player Double]
    get p = [M.lookupDefault defaultPlayer p players]

countDraws v = V.length $ V.filter (\row -> row!3 == row!4) v

-- | Takes a model a set of training games to build a table of scores.
--
-- The score table can be used later to lookup observed results given the mean
-- difference between to team skills.
buildGoalTable :: Model -> V.Vector (V.Vector String) -> ScoreLookup
buildGoalTable model v = V.toList $ V.filter checkProper
    $ V.map (\row -> entry (row!1) (row!2) (row!3) (row!4)) v
  where
    checkProper (mu, (score1, score2)) = mu > eps && score1 > score2 ||
      abs mu < eps && score1 == score2 ||
      mu < -eps && score1 < score2
    entry :: String -> String -> String -> String -> (Double, (Int, Int))
    entry player1Name player2Name score1 score2 = (mu, (read score1, read score2))
      where
        (mu, sigma2) = toMuSigma2 $ predict [get player1Name] [get player2Name]

        get p = M.lookupDefault defaultPlayer p model

-- | Predict the result of a game given a model and a score table.
queryRow :: Model
         -> ScoreLookup
         -> V.Vector String
         -> (Int, String, String, Int, Int, Double, Double)
queryRow model table row =
    ( 0
    , player1Name
    , player2Name
    , fst $ snd best
    , snd $ snd best
    , mu
    , sigma2)
  where
    player1Name = row!1
    player2Name = row!2

    best = bestScore mu

    -- | Finds the closest result from the score table with respect to the mean
    -- of the belief mu.
    --
    -- TODO Could be improved with a binary search.
    bestScore mu = foldl keepBetter (head table) table
      where
        keepBetter t1@(d, s) t2@(d_, s_)
          | abs (d_ - mu) < abs (d - mu) = t2
          | otherwise                    = t1

    (mu, sigma2) = toMuSigma2 $ predict [get player1Name] [get player2Name]

    get p = M.lookupDefault undefined p model

-- | Find the strongest team with a similar heuristic as in the original
-- TrueSkill paper.
findBestPlayer name player p@(name_, value_)
  | value_ < value = (name, value)
  | otherwise      = p
  where
    (mu, sigma2) = toMuSigma2 $ (view skill player)
    value = mu - 3 * sqrt sigma2

main = do
    [trainFile, goalFile] <- getArgs
    rawTrainData <- BL.readFile trainFile
    rawGoalData <- BL.readFile goalFile

    rawQueryData <- BL.getContents

    results <- runEitherT $ do
        trainData <- decodeCsv rawTrainData

        let model = V.foldl' updateModel M.empty trainData
        lift $ hPutStrLn stderr $ show model

        let (best, value) = M.foldrWithKey
                              findBestPlayer ("noland", -100) model
        lift $ putStrLn $ best ++ ": " ++ show value

        goalData <- decodeCsv rawGoalData
        let goalTable = buildGoalTable model goalData

        queryData <- decodeCsv rawQueryData

        return $ V.map (queryRow model goalTable) queryData

    report results


  where
    report (Left err)      = putStrLn err
    report (Right results) = BL.putStr $ encode $ V.toList results

    decodeCsv = hoistEither . decode NoHeader

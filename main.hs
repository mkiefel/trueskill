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

import           TrueSkill ( predict
                           , update
                           , eps
                           , defaultPlayer
                           , toMuSigma2
                           , skill
                           , Player
                           , Result(..) )


mangleRow :: M.HashMap String Player -> V.Vector String -> M.HashMap String Player
mangleRow players row = M.insert player2Name player2 $ M.insert player1Name player1 players
  where
    player1Name = row!1
    player2Name = row!2
    gameID = read $ row!5

    ([player1], [player2]) = update gameID (get player1Name) (get player2Name) result

    result
      | ((score $ row!3) > (score $ row!4))  = Won
      | ((score $ row!3) == (score $ row!4)) = Draw
      | otherwise                            = Lost

    score :: String -> Int
    score s = read s

    get :: String -> [Player]
    get p = [M.lookupDefault defaultPlayer p players]

countDraws v = V.length $ V.filter (\row -> row!3 == row!4) v

buildGoalTable :: M.HashMap String Player -> V.Vector (V.Vector String) -> [(Double, (Int, Int))]
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

queryRow :: M.HashMap String Player -> [(Double, (Int, Int))] -> V.Vector String -> (Int, String, String, Int, Int, Double, Double)
queryRow model table row = (eval, player1Name, player2Name, fst $ snd best, snd $ snd best, mu, sigma2)
  where
    player1Name = row!1
    player2Name = row!2

    best = bestScore mu

    bestScore sample = foldl keepBetter (head table) table
      where
        keepBetter t1@(d, s) t2@(d_, s_)
          | abs (d_ - sample) < abs (d - sample) = t2
          | otherwise                            = t1

    (mu, sigma2) = toMuSigma2 $ predict [get player1Name] [get player2Name]

    get p = M.lookupDefault undefined p model

    -- evaluation
    eval
        | V.length row < 4 = 0
        | score1 == score1_ && score2 == score2_ = 4
        | score1 /= score2 && (score1 - score2) == (score1_ - score2_) = 3
        | score1 == score2 && (score1 - score2) == (score1_ - score2_) = 2
        | (score1 > score2 && score1_ > score2_) || (score1 < score2 && score1_ < score2_) = 2
        | otherwise = 0
      where
        score1 = read (row!3)
        score2 = read (row!4)

        score1_ = fst $ snd best
        score2_ = snd $ snd best

findBestPlayer name player p@(name_, value_)
  | value_ < value = (name, value)
  | otherwise      = p
  where
    (mu, sigma2) = toMuSigma2 $ (view skill player)
    value = mu - 3 * sqrt sigma2

main = do
    [trainFile,goalFile] <- getArgs
    csvData <- BL.readFile trainFile
    case decode NoHeader csvData of
      Left err -> putStrLn err
      Right v -> do
        {-print ((fromIntegral $ countDraws v) / (fromIntegral $ V.length v))-}
        let model = V.foldl' mangleRow M.empty v
        hPutStrLn stderr $ show model

        let (best, value) = M.foldrWithKey findBestPlayer ("noland", -100) model
        putStrLn $ best ++ ": " ++ show value

        csvData <- BL.readFile goalFile
        case decode NoHeader csvData of
          Left err -> putStrLn err
          Right g -> do
            let goalTable = buildGoalTable model g
            {-hPutStrLn stderr $ show goalTable-}

            queryData <- BL.getContents
            let results = case decode NoHeader queryData of
                            Left err -> V.empty
                            Right q -> V.map (queryRow model goalTable) q

            BL.putStr $ encode $ V.toList results


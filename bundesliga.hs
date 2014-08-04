{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Main where

import qualified Data.HashMap.Lazy as M
import           Data.Csv ( HasHeader(..), decode, encode )
import qualified Data.Vector as V
import           Data.Vector ( (!) )
import qualified Data.ByteString.Lazy as BL
import           System.Environment ( getArgs )
import           System.IO ( stderr, hPutStrLn )
import           Control.Lens
import           Data.List ( foldl' )
import           Control.Monad.Trans.Either ( EitherT(..)
                                            , runEitherT
                                            , hoistEither )
import           Control.Monad.Trans.Class ( lift )

import           TrueSkill ( predict
                           , update
                           , defaultPlayer
                           , toMuSigma2
                           , toResult
                           , skill
                           , Player
                           , Result(..) )

type Model = M.HashMap String (Player Double)

data Game = Game
  { _team1  :: ![String]
  , _team2  :: ![String]
  , _result :: !Result
  , _gameID :: !Int
  }
makeLenses ''Game

-- | Transforms a CSV row into a game.
parseGame :: V.Vector String -> Game
parseGame row = Game team1 team2 result gameID
  where
    team1 = map (\i -> row!i) [7..7+10]
    team2 = map (\i -> row!i) [18..18+10]
    gameID = read $ row!0

    [score1String, score2String] = splitBy ':' $ head $ splitBy ' ' $ row!6
    score = (read $ score1String, read $ score2String) :: (Int, Int)

    result
      | ((fst score)  > (snd score)) = Won
      | ((fst score) == (snd score)) = Draw
      | otherwise                    = Lost

    splitBy delimiter = foldr f [[]]
      where
        f c l@(x:xs) | c == delimiter = []:l
                     | otherwise = (c:x):xs

updateModel :: Model -> Game -> Model
updateModel players game = updatedModel
  where
    (updatedTeam1, updatedTeam2) = update
                                    (game ^. gameID)
                                    (map get $ game ^. team1)
                                    (map get $ game ^. team2)
                                    (game ^. result)

    updatedModel = foldl' put players
                    $ zip (game ^. team1 ++ game ^. team2)
                          (updatedTeam1 ++ updatedTeam2)

    put :: Model -> (String, Player Double) -> Model
    put m (p, player) = M.insert p player m

    get :: String -> Player Double
    get p = M.lookupDefault defaultPlayer p players

findBestPlayer name player p@(name_, value_)
  | value_ < value = (name, value)
  | otherwise      = p
  where
    (mu, sigma2) = toMuSigma2 $ (view skill player)
    value = mu - 3 * sqrt sigma2

trainModel :: V.Vector Game -> Model
trainModel games =
    V.foldl' updateModel M.empty games

testModel :: Model -> V.Vector Game -> V.Vector Result
testModel players games =
    V.map (\g -> toResult $ predict (map get $ g ^. team1)
                                    (map get $ g ^. team2)) games
  where
    get :: String -> Player Double
    get p = M.lookupDefault defaultPlayer p players

main = do
    [trainFile, testFile] <- getArgs
    trainFileData <- BL.readFile trainFile
    testFileData <- BL.readFile testFile

    results <- runEitherT $ do
        trainData <- decodeCsv trainFileData

        let model = trainModel trainData
        let (best, value) = M.foldrWithKey
                              findBestPlayer ("noland", -100) model

        lift $ putStrLn $ best ++ ": " ++ show value

        testData <- decodeCsv testFileData

        let prediction = testModel model testData

        return $ V.length
               $ V.filter (\(g, p) -> (g ^. result) == p)
               $ V.zip testData prediction
    report results

  where
    report (Left err)      = putStrLn err
    report (Right results) = print results

    decodeCsv :: Monad m => BL.ByteString -> EitherT String m (V.Vector Game)
    decodeCsv = (V.map parseGame `fmap`) . hoistEither . decode NoHeader

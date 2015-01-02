{-# LANGUAGE TemplateHaskell #-}
module Types ( Game
             , Model
             , readGamesFromCsv
             , team1
             , team2
             , result
             , gameID
             )where

import           Control.Lens
import qualified Data.ByteString.Lazy as BL
import           Data.Csv ( HasHeader(..)
                          , decode
                          )
import qualified Data.HashMap.Strict as M
import qualified Data.Vector as V
import           Data.Vector ( (!) )

import           TrueSkill ( Player
                           , Result(..)
                           )

type Model d = M.HashMap String (Player d)

data Game = Game
  { _team1  :: ![String]
  , _team2  :: ![String]
  , _result :: !Result
  , _gameID :: !Int
  } deriving Show
makeLenses ''Game

readGamesFromCsv :: FilePath -> IO (Either String (V.Vector Game))
readGamesFromCsv path = BL.readFile path >>= return . decodeCsv
  where
    decodeCsv = (V.map parseGame `fmap`) . decode NoHeader

-- | Transforms a CSV row into a game.
parseGame :: V.Vector String -> Game
parseGame row = Game team1' team2' result' gameID'
  where
    team1' = map (\i -> row!i) [8..8+10]
    team2' = map (\i -> row!i) [19..19+10]
    gameID' = read $ row!0

    [score1String, score2String] = splitBy ':' $ head $ splitBy ' ' $ row!7
    result' = Result (read score1String, read score2String)

    splitBy delimiter = foldr f [[]]
      where
        f c l@(x:xs) | c == delimiter = []:l
                     | otherwise      = (c:x):xs
        f _ _                         = undefined

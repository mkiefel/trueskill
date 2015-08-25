{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Types ( Game
             , Model
             , readGamesFromCsv
             , team1
             , team2
             , result
             , gameID
             , gameOdds
             )where

import           Control.Applicative ( (<$>), (<*>) )
import           Control.Lens hiding ( (.=) )
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import           Data.ByteString.Char8 ( pack )
import           Data.Csv ( decodeByName
                          , FromNamedRecord(..)
                          , (.:)
                          )
import qualified Data.HashMap.Strict as M
import qualified Data.Vector as V

import           TrueSkill ( Player
                           , Result(..)
                           )

type Model d = M.HashMap String (Player d)

data Game = Game
  { _team1  :: ![String]
  , _team2  :: ![String]
  , _result :: !Result
  , _gameOdds :: !(Double, Double, Double)
  , _gameID :: !String
  } deriving Show
makeLenses ''Game

instance FromNamedRecord Game where
  parseNamedRecord m = do
    goalsHome <- m.: "Goals_Home"
    goalsGuest <- m.: "Goals_Guest"

    let lookupPlayers base = mapM (\p -> m .: (base `B.append` (pack $ show p)))
                             ([1..11] :: [Int])
    teamHome <- lookupPlayers "Player_Home_Name_"
    teamGuest <- lookupPlayers "Player_Guest_Name_"

    -- teamHome <- (:[]) <$> m.: "Home"
    -- teamGuest <- (:[]) <$> m.: "Guest"

    odds <- (,,) <$> m .: "OddsMeanH" <*> m .: "OddsMeanD" <*> m .: "OddsMeanA"

    Game teamHome teamGuest (Result (goalsHome, goalsGuest)) odds
      <$> m.: "Game_ID"

readGamesFromCsv :: FilePath -> IO (Either String (V.Vector Game))
readGamesFromCsv path =BL.readFile path >>= (\f -> return $ snd <$> decodeByName f)

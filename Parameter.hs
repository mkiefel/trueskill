{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Parameter ( Knobs(..)
                 , readKnobs
                 , writeKnobs
                 ) where

import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy as BL

data Knobs = Knobs
             { getDefaultMuOffense :: Double
             , getDefaultSigmaOffense :: Double
             , getDefaultMuDefense :: Double
             , getDefaultSigmaDefense :: Double
             , getSigmaOffense :: Double
             , getSigmaDefense :: Double
             , getMessagePasses :: Int
             }
$(deriveJSON defaultOptions{fieldLabelModifier = drop 3} ''Knobs)

readKnobs :: FilePath -> IO (Maybe Knobs)
readKnobs path = BL.readFile path >>= return . decode

writeKnobs :: FilePath -> Knobs -> IO ()
writeKnobs path knobs = BL.writeFile path $ encode knobs

--
-- Parse YAML configuration.
--
-- Copyright © 2014–2016 Mark Karpov
--
-- MIDA is free software: you can redistribute it and/or modify it under the
-- terms of the GNU General Public License as published by the Free Software
-- Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- MIDA is distributed in the hope that it will be useful, but WITHOUT ANY
-- WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
-- details.
--
-- You should have received a copy of the GNU General Public License along
-- with this program. If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE TemplateHaskell #-}

module Mida.Configuration
  ( MidaConfig (..)
  , parseMidaConfig
  , def )
where

import Control.Monad.IO.Class
import Data.Aeson (withObject)
import Data.Default
import Data.Yaml
import Numeric.Natural
import Path

-- | MIDA configuration.

data MidaConfig = MidaConfig
  { configPrevLen :: Natural       -- ^ Length of preview principles
  , configSrcFile :: Path Rel File -- ^ Name of current source file
  , configProg    :: Natural       -- ^ Program to use for preview
  , configTempo   :: Natural       -- ^ Tempo to use for preview
  , configPrompt  :: String        -- ^ REPL prompt
  , configVerbose :: Bool          -- ^ Verbose mode?
  , configPrvCmd  :: String        -- ^ Command to use for preview
  , configProgOp  :: String        -- ^ Option to set program for preview
  , configTempoOp :: String        -- ^ Option to set tempo for preview
  } deriving (Eq, Show)

instance Default MidaConfig where
  def = MidaConfig
    { configPrevLen = 18
    , configSrcFile = $(mkRelFile "foo.da")
    , configProg    = 0
    , configTempo   = 120
    , configPrompt  = "> "
    , configVerbose = True
    , configPrvCmd  = "timidity"
    , configProgOp  = "--force-program"
    , configTempoOp = "--adjust-tempo"
    }

instance FromJSON MidaConfig where
  parseJSON = withObject "MIDA Configuration" $ \o -> do
    let ω f g n = do
          mval <- o .:? n
          case mval of
            Nothing -> return (f def)
            Just val -> g val
        ξ x = case parseRelFile x of
                Nothing -> fail $ "cannot parse relative path: " ++ show x
                Just path -> return path
        τ :: Int -> Parser Natural
        τ x = if x >= 0
                then return (fromIntegral x)
                else fail $ "the value must be non-negative: " ++ show x
    configPrevLen <- ω configPrevLen τ "prevlen"
    configSrcFile <- ω configSrcFile ξ "src"
    configProg    <- ω configProg    τ "prog"
    configTempo   <- ω configTempo   τ "tempo"
    configPrompt  <- ω configPrompt  return "prompt"
    configVerbose <- ω configVerbose return "verbose"
    configPrvCmd  <- ω configPrvCmd  return "prvcmd"
    configProgOp  <- ω configProgOp  return "progop"
    configTempoOp <- ω configTempoOp return "tempop"
    return MidaConfig {..}

-- | Parse configuration from specified YAML file.

parseMidaConfig :: MonadIO m => Path b File -> m (Either String MidaConfig)
parseMidaConfig path = liftIO $
  either (Left . prettyPrintParseException) Right
    <$> decodeFileEither (toFilePath path)

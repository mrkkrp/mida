-- -*- Mode: Haskell; -*-
--
-- This module describes monad for interactive REPL and some basic
-- functions.
--
-- Copyright © 2014, 2015 Mark Karpov
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

module Mida.Interaction.Base
  ( MidaSt  (..)
  , MidaCfg (..)
  , Mida
  , runMida
  , defaultSeed
  , defaultQuarter
  , defaultBeats
  , processDef )
where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Formatting
import Mida.Language
import Numeric.Natural
import Path

-- | MIDA REPL state.

data MidaSt = MidaSt
  { stPrevLen :: Natural -- ^ Length of preview principles
  , stSrcFile :: Path Abs File -- ^ Name of current source file
  , stProg    :: Natural -- ^ Program to use for preview
  , stTempo   :: Natural -- ^ Tempo to use for preview
  }

-- | MIDA REPL configuration.

data MidaCfg = MidaCfg
  { cfgPrompt  :: String -- ^ REPL prompt
  , cfgVerbose :: Bool -- ^ Verbose mode?
  , cfgPrvCmd  :: String -- ^ Command to use for preview
  , cfgProgOp  :: String -- ^ Option to set program for preview
  , cfgTempoOp :: String -- ^ Option to set tempo for preview
  }

-- | A synonym for MIDA monad stack.

type Mida = StateT MidaSt (ReaderT MidaCfg (MidaEnv IO))

-- | Run MIDA monad stack.

runMida
  :: Mida a            -- ^ MIDA monad stack
  -> MidaSt            -- ^ Initial state
  -> MidaCfg           -- ^ Configuration
  -> IO a
runMida m st cfg = runMidaEnv (runReaderT (evalStateT m st) cfg)

-- | Default seed for random number generator.

defaultSeed :: Natural
defaultSeed = 0

-- | Default number of ticks in quarter.

defaultQuarter :: Natural
defaultQuarter = 24

-- | Default duration of MIDI file to render, in quarter notes.

defaultBeats :: Natural
defaultBeats = 16

-- | Process a principle definition.

processDef :: (HasEnv m, MonadIO m)
  => String
  -> SyntaxTree
  -> m ()
processDef n t = do
  recursive <- checkRecur n t
  if recursive
  then liftIO $
    fprint ("Rejected recursive definition for «" % string % "».\n") n
  else do addDef n t
          liftIO $ fprint ("• «" % string % "»\n") n

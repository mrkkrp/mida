--
-- This is entry point of Mida.Lang library.
--
-- Copyright © 2014–2017 Mark Karpov
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

module Mida.Language
  ( SyntaxTree
  , Sel     (..)
  , Statement (..)
  , Principle
  , Element (..)
  , MidaEnv
  , HasEnv  (..)
  , runMidaEnv
  , addDef
  , remDef
  , clearDefs
  , getPrin
  , getSrc
  , fullSrc
  , getRefs
  , purgeEnv
  , checkRecur
  , evalDef
  , eval
  , toPrin )
where

import Mida.Language.SyntaxTree (SyntaxTree, Sel (..), Statement (..))
import Mida.Language.Element (Principle, Element (..))
import Mida.Language.Environment
import Mida.Language.Eval (evalDef, eval, toPrin)

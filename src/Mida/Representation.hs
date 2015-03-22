-- -*- Mode: Haskell; -*-
--
-- This module binds together syntax tree and textual representation of MIDA
-- language.
--
-- Copyright (c) 2014, 2015 Mark Karpov
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

module Mida.Representation
    ( Statement (..)
    , probeMida
    , parseMida
    , showStatement
    , showSyntaxTree
    , showPrinciple )
where

import Mida.Representation.Parser
import Mida.Representation.Show

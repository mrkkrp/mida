-- -*- Mode: Haskell; -*-
--
-- Textual representation of basic elements in MIDA language.
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

module Mida.Representation.Base
    ( noteAlias
    , commentLine
    , productOp
    , divisionOp
    , sumOp
    , diffOp
    , loopOp
    , rotationOp
    , reverseOp
    , rangeOp
    , defOp
    , figures )
where

noteAlias :: [String]
noteAlias = zipWith f names ([0..127] :: [Int])
    where f n i = n ++ show (i `div` 12)
          names = cycle ["c","cs","d","ds","e","f","fs","g","gs","a","as","b"]

commentLine :: String
commentLine = "#"

productOp :: String
productOp = "*"

divisionOp :: String
divisionOp = "/"

sumOp :: String
sumOp = "+"

diffOp :: String
diffOp = "-"

loopOp :: String
loopOp = "$"

rotationOp :: String
rotationOp = "^"

reverseOp :: String
reverseOp = "@"

rangeOp :: String
rangeOp = ".."

defOp :: String
defOp = "="

figures :: [String]
figures = ["_ud","_du","_u","_d"]

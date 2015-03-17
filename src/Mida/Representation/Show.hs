-- -*- Mode: Huskily; -*-
--
-- Here we describe how to transform `Element' and `Sel' instances to their
-- textual representation.
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

module Mida.Representation.Show
    ( showSyntaxTree
    , showPrinciple )
where

import Control.Arrow ((***), (>>>))
import Data.List (intercalate)

import Mida.Language.SyntaxTree (SyntaxTree, Sel (..))
import Mida.Language.Element (Principle, Element (..))
import qualified Mida.Representation.Base as B

showSyntaxTree :: SyntaxTree -> String
showSyntaxTree = cm f
    where cm g xs = intercalate " " $ map g xs
          f (Value      x) = show x
          f (Section    x) = "[" ++ cm f x ++ "]"
          f (Multi      x) = "{" ++ cm f x ++ "}"
          f (CMulti     x) = "{" ++ cm (c *** v >>> uncurry (++)) x ++ "}"
          f (Reference  x) = x
          f (Range    x y) = show x ++ B.rangeOp ++ show y
          f (Product  x y) = f x ++ B.productOp ++ show y
          f (Division x y) = f x ++ B.divisionOp ++ show y
          f (Sum      x y) = f x ++ B.sumOp ++ show y
          f (Diff     x y) = f x ++ B.diffOp ++ show y
          f (Loop     x y) = f x ++ B.loopOp ++ show y
          f (Rotation x y) = f x ++ B.rotationOp ++ show y
          f (Reverse    x) = B.reverseOp ++ show x
          c (Multi      x) = "<" ++ cm f x ++ "> "
          c _              = "???"
          v (Multi      x) = cm f x
          v _              = "???"

showPrinciple :: Principle -> String
showPrinciple = showSyntaxTree . toSyntaxTree

toSyntaxTree :: Principle -> SyntaxTree
toSyntaxTree = map f
    where f (Val  x) = Value x
          f (Sec  x) = Section $ map f x
          f (Mul  x) = Multi $ map f x
          f (CMul x) = CMulti $ map (f *** f) x

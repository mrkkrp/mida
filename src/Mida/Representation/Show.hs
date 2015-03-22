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
    ( showStatement
    , showSyntaxTree
    , showPrinciple )
where

import Control.Applicative ((<$>))
import Control.Arrow ((***), (>>>))

import Mida.Language (SyntaxTree, Sel (..), Principle, Element (..))
import Mida.Representation.Parser (Statement (..))
import qualified Mida.Representation.Base as B

showStatement :: Statement -> String
showStatement (Definition n t _) = n ++ B.definitionOp ++ showSyntaxTree t
showStatement (Exposition t)     = showSyntaxTree t

showSyntaxTree :: SyntaxTree -> String
showSyntaxTree = cm f
    where cm g xs = unwords $ g <$> xs
          f (Value      x) = show x
          f (Section    x) = "[" ++ cm f x ++ "]"
          f (Multi      x) = "{" ++ cm f x ++ "}"
          f (CMulti     x) = "{" ++ cm (c *** v >>> uncurry (++)) x ++ "}"
          f (Reference  x) = x
          f (Range    x y) = show x ++ B.rangeOp ++ show y
          f (Product  x y) = f x ++ B.productOp ++ f y
          f (Division x y) = f x ++ B.divisionOp ++ f y
          f (Sum      x y) = f x ++ B.sumOp ++ f y
          f (Diff     x y) = f x ++ B.diffOp ++ f y
          f (Loop     x y) = f x ++ B.loopOp ++ f y
          f (Rotation x y) = f x ++ B.rotationOp ++ f y
          f (Reverse    x) = B.reverseOp ++ f x
          c xs             = "<" ++ cm f xs ++ "> "
          v                = cm f

showPrinciple :: Principle -> String
showPrinciple = showSyntaxTree . toSyntaxTree

toSyntaxTree :: Principle -> SyntaxTree
toSyntaxTree = map f
    where f (Val  x) = Value x
          f (Sec  x) = Section $ f <$> x
          f (Mul  x) = Multi   $ f <$> x
          f (CMul x) = CMulti  $ (toSyntaxTree *** toSyntaxTree) <$> x

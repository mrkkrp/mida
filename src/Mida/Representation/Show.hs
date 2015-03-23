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
    , showDefinition
    , showSyntaxTree
    , showPrinciple )
where

import Control.Applicative ((<$>))
import Control.Arrow ((***), (>>>))

import Mida.Language.Element (Principle, Element (..))
import Mida.Language.SyntaxTree (SyntaxTree, Sel (..))
import Mida.Representation.Parser (Statement (..))
import qualified Mida.Representation.Base as B

showStatement :: Statement -> String
showStatement (Definition n t) = showDefinition n t
showStatement (Exposition   t) = showSyntaxTree t

showDefinition :: String -> SyntaxTree -> String
showDefinition n t = n ++ pad B.defOp ++ showSyntaxTree t

showSyntaxTree :: SyntaxTree -> String
showSyntaxTree t = cm f t ++ "\n"
    where cm g xs = unwords $ g <$> xs
          p x@(Value     _) = f x
          p x@(Section   _) = f x
          p x@(Multi     _) = f x
          p x@(CMulti    _) = f x
          p x@(Reference _) = f x
          p x@(Range   _ _) = f x
          p x               = "(" ++ f x ++ ")"
          f (Value      x) = show x
          f (Section    x) = "[" ++ cm f x ++ "]"
          f (Multi      x) = "{" ++ cm f x ++ "}"
          f (CMulti     x) = "{" ++ cm (c *** v >>> uncurry (++)) x ++ "}"
          f (Reference  x) = x
          f (Range    x y) = show x ++ B.rangeOp ++ show y
          f (Product  x y) = p x ++ pad B.productOp ++ p y
          f (Division x y) = p x ++ pad B.divisionOp ++ p y
          f (Sum      x y) = p x ++ pad B.sumOp ++ p y
          f (Diff     x y) = p x ++ pad B.diffOp ++ p y
          f (Loop     x y) = p x ++ pad B.loopOp ++ p y
          f (Rotation x y) = p x ++ pad B.rotationOp ++ p y
          f (Reverse    x) = B.reverseOp ++ p x
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

pad :: String -> String
pad op = ' ':op ++ " "

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

{-# LANGUAGE OverloadedStrings #-}

module Mida.Representation.Show
    ( showStatement
    , showDefinition
    , showSyntaxTree
    , showPrinciple )
where

import Control.Applicative ((<$>))
import Control.Arrow ((***), (>>>))
import qualified Data.Text as T

import Mida.Language.Element (Principle, Element (..))
import Mida.Language.SyntaxTree (SyntaxTree, Sel (..))
import Mida.Representation.Parser (Statement (..))
import qualified Mida.Representation.Base as B
import qualified Text.Show.Text as T

showStatement :: Statement -> T.Text
showStatement (Definition n t) = showDefinition n t
showStatement (Exposition   t) = showSyntaxTree t

showDefinition :: String -> SyntaxTree -> T.Text
showDefinition n t = T.pack n `T.append` pad B.defOp `T.append` showSyntaxTree t

showSyntaxTree :: SyntaxTree -> T.Text
showSyntaxTree t = cm f t `T.snoc` '\n'
    where
      cm g xs = T.unwords $ g <$> xs
      p x@(Value     _) = f x
      p x@(Section   _) = f x
      p x@(Multi     _) = f x
      p x@(CMulti    _) = f x
      p x@(Reference _) = f x
      p x@(Range   _ _) = f x
      p x               = '(' `T.cons` f x `T.snoc` ')'
      f (Value      x) = T.show x
      f (Section    x) = '[' `T.cons` cm f x `T.snoc` ']'
      f (Multi      x) = '{' `T.cons` cm f x `T.snoc` '}'
      f (CMulti     x) = '{' `T.cons`
                         cm (c *** cm f >>> uncurry T.append) x `T.snoc` '}'
      f (Reference  x) = T.pack x
      f (Range    x y) = T.show x `T.append`
                         T.pack B.rangeOp `T.append` T.show y
      f (Product  x y) = p x `T.append` pad B.productOp `T.append` p y
      f (Division x y) = p x `T.append` pad B.divisionOp `T.append` p y
      f (Sum      x y) = p x `T.append` pad B.sumOp `T.append` p y
      f (Diff     x y) = p x `T.append` pad B.diffOp `T.append` p y
      f (Loop     x y) = p x `T.append` pad B.loopOp `T.append` p y
      f (Rotation x y) = p x `T.append` pad B.rotationOp `T.append` p y
      f (Reverse    x) = T.pack B.reverseOp `T.append` p x
      c xs             = '<' `T.cons` cm f xs `T.append` "> "

showPrinciple :: Principle -> T.Text
showPrinciple = showSyntaxTree . toSyntaxTree

toSyntaxTree :: Principle -> SyntaxTree
toSyntaxTree = map f
    where f (Val  x) = Value x
          f (Sec  x) = Section $ f <$> x
          f (Mul  x) = Multi   $ f <$> x
          f (CMul x) = CMulti  $ (toSyntaxTree *** toSyntaxTree) <$> x

pad :: String -> T.Text
pad op = ' ' `T.cons` T.pack op `T.snoc` ' '

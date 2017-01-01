--
-- This module defines abstract syntax tree of MIDA language.
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

module Mida.Language.SyntaxTree
  ( SyntaxTree
  , Sel (..)
  , Statement (..) )
where

import Data.Char (isLetter, isAlphaNum)
import Data.List.NonEmpty (NonEmpty (..))
import Numeric.Natural
import Test.QuickCheck

-- | Syntax tree in our case is just a collection of syntactic elements.

type SyntaxTree = [Sel]

-- | Syntactic element corresponds to language tokens. Some of them have
-- direct corresponding constructor in 'Mida.Language.Element.Element',
-- others have to be simplified first.

data Sel
  = Value     Natural  -- ^ Literal value
  | Section   [Sel]    -- ^ Section
  | Multi     [Sel]    -- ^ Multivalue
  | CMulti    (NonEmpty ([Sel], [Sel])) -- ^ Conditional multivalue
  | Reference String   -- ^ Reference (name of variable)
  | Range     Natural Natural -- ^ Range of values
  | Product   Sel Sel  -- ^ Product of principles
  | Division  Sel Sel  -- ^ Division of principles
  | Sum       Sel Sel  -- ^ Sum of principles
  | Diff      Sel Sel  -- ^ Subtraction of principles
  | Loop      Sel Sel  -- ^ Loop
  | Rotation  Sel Sel  -- ^ Rotation
  | Reverse   Sel      -- ^ Reversed principle
    deriving (Eq, Show)

instance Arbitrary Sel where
  arbitrary = sized arbitrarySel

arbitrarySel :: Int -> Gen Sel
arbitrarySel 0 =
  oneof [ Value     <$> nonNegative
        , Reference <$> identifier
        , Range     <$> nonNegative <*> nonNegative ]
  where nonNegative = getNonNegative <$> arbitrary
arbitrarySel n =
  oneof [ Section  <$> listSel
        , Multi    <$> listSel
        , CMulti   <$> listCnd
        , Product  <$> leafSel <*> leafSel
        , Division <$> leafSel <*> leafSel
        , Sum      <$> leafSel <*> leafSel
        , Diff     <$> leafSel <*> leafSel
        , Loop     <$> leafSel <*> leafSel
        , Rotation <$> leafSel <*> leafSel
        , Reverse  <$> leafSel ]
  where cnSel d = arbitrarySel (n `div` d)
        vcSel d = listOf (cnSel $ d * 20)
        leafSel = cnSel 10
        listSel = vcSel 20
        listCnd =
          let x = (,) <$> vcSel 25 <*> vcSel 25
          in (:|) <$> x <*> listOf x

-- | Statement can be either definition or exposition. Expositions are only
-- used in REPL.

data Statement
  = Definition String SyntaxTree
  | Exposition SyntaxTree
    deriving (Eq, Show)

instance Arbitrary Statement where
  arbitrary =
    oneof [ Definition <$> identifier <*> arbitrary
          , Exposition <$> arbitrary ]

----------------------------------------------------------------------------
-- Helpers

identifier :: Gen String
identifier = (:) <$> ch0 <*> chN
  where ch0 = u isLetter
        chN = listOf $ u isAlphaNum
        u f = frequency [(1, return '_'), (74, arbitrary `suchThat` f)]

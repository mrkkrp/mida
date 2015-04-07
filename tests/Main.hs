-- -*- Mode: Haskell; -*-
--
-- QuickCheck tests for MIDA.
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

{-
  TODO:
  - parser for configuration files;
  - translation from syntax tree to internal language;
  - process of translation Principle -> [Int];
  - generation of MIDI files.
-}

{-# OPTIONS -fno-warn-orphans #-}

module Main (main) where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((&&&), (>>>))
import Data.Char (isLetter, isAlphaNum)

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Mida.Language (Sel (..))
import Mida.Representation
    ( Statement (..)
    , probeMida
    , parseMida
    , showStatement )

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [ testGroup "Printer and Parser"
      [ testProperty "Valid MIDA Probe"               prop_valid_probe
      , testProperty "Printer and Parser Consistency" prop_pp_consistency ] ]

prop_valid_probe :: Statement -> Bool
prop_valid_probe = probeMida . showStatement

prop_pp_consistency :: Statement -> Bool
prop_pp_consistency = id &&& (parseMida "" . showStatement) >>> check
    where check (x, Right (y:[])) = x == y
          check _                 = False

instance Arbitrary Statement where
    arbitrary =
        oneof [ Definition <$> alphaNumIdentifier <*> arbitrary
              , Exposition <$> arbitrary ]

instance Arbitrary Sel where
    arbitrary = sized arbitrarySel

arbitrarySel :: Int -> Gen Sel
arbitrarySel 0 =
    oneof [ Value     <$> positive
          , Reference <$> alphaNumIdentifier
          , Range     <$> positive <*> positive ]
    where positive = arbitrary `suchThat` (>= 0)
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
          vcSel d = arbitrarySizedIntegral `suchThat` (>= 0)
                    >>= \s -> vectorOf s (cnSel $ d * s)
          leafSel = cnSel 2
          listSel = vcSel 1
          listCnd = arbitrarySizedIntegral `suchThat` (>= 1)
                    >>= \s -> vectorOf s $ (,) <$> vcSel s <*> vcSel s

alphaNumIdentifier :: Gen String
alphaNumIdentifier = (:) <$> ch0 <*> chN
    where ch0 = arbitrary `suchThat` underscoreOr isLetter
          chN = arbitrary `suchThat` all (underscoreOr isAlphaNum)
          underscoreOr f x = x == '_' || f x

-- -*- Mode: Haskell; -*-
--
-- This module describes internal representation of principle as list of
-- elements.
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

module Mida.Language.Element
    ( Principle
    , Elt
    , Element (..) )
where

import Control.Applicative (Applicative, pure, (<$>), (<*>))
import Control.Arrow ((***), (>>>))
import Data.Monoid (mappend, mconcat)
import qualified Data.Foldable as F (Foldable, foldMap)

type Principle = [Elt]
type Elt       = Element Int

data Element a
    = Val  a
    | Sec  [Element a]
    | Mul  [Element a]
    | CMul [(Element a, Element a)]
      deriving (Show)

instance Functor Element where
    f `fmap` (Val  x) = Val  $ f x
    f `fmap` (Sec  x) = Sec  $ (f <$>) <$> x
    f `fmap` (Mul  x) = Mul  $ (f <$>) <$> x
    f `fmap` (CMul x) = CMul $ ((f <$>) *** (f <$>)) <$> x

instance Applicative Element where
    pure = Val
    (Val  f) <*> x = f <$> x
    (Sec  f) <*> x = Sec  $ (<*> x) <$> f
    (Mul  f) <*> x = Mul  $ (<*> x) <$> f
    (CMul f) <*> x = CMul $ ((<*> x) *** (<*> x)) <$> f

instance F.Foldable Element where
    foldMap f (Val  x) = f x
    foldMap f (Sec  x) = mconcat $ (F.foldMap f) <$> x
    foldMap f (Mul  x) = mconcat $ (F.foldMap f) <$> x
    foldMap f (CMul x) = mconcat $ (F.foldMap f *** F.foldMap f >>>
                                    uncurry mappend) <$> x

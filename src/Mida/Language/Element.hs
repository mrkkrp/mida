-- -*- Mode: Haskell; -*-
--
-- This module describes internal representation of principle as list of
-- elements.
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

{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE DeriveFoldable #-}

module Mida.Language.Element
    ( Principle
    , Elt
    , Element (..) )
where

import Control.Applicative (Applicative, pure, (<$>), (<*>))
import Control.Arrow ((***))
import Data.Foldable

type Principle = [Elt]
type Elt       = Element Int

data Element a
    = Val  a
    | Sec  [Element a]
    | Mul  [Element a]
    | CMul [([Element a], [Element a])]
      deriving (Eq, Show, Functor, Foldable)

instance Applicative Element where
    pure           = Val
    (Val  f) <*> x = f <$> x
    (Sec  f) <*> x = Sec  $ (<*> x) <$> f
    (Mul  f) <*> x = Mul  $ (<*> x) <$> f
    (CMul f) <*> x = CMul $ (((<*> x) <$>) *** ((<*> x) <$>)) <$> f

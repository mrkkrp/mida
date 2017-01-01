--
-- This module describes internal representation of principle as list of
-- elements.
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

{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor  #-}

module Mida.Language.Element
  ( Principle
  , Element (..) )
where

import Control.Arrow ((***))
import Data.List.NonEmpty (NonEmpty (..))
import Numeric.Natural

-- | Collection of elements for evaluation, representation of some aspect of
-- voice.

type Principle = [Element Natural]

-- | Fundamental type representing an atom for evaluation.

data Element a
  = Val  a             -- ^ Single value, evaluates to itself
  | Sec  [Element a]   -- ^ Universal container for other values
  | Mul  [Element a]   -- ^ Multivalue, the way to introduce varying elements
  | CMul (NonEmpty ([Element a], [Element a])) -- ^ Conditional multivalue
    deriving (Eq, Show, Functor, Foldable)

instance Applicative Element where
  pure           = Val
  (Val  f) <*> x = f <$> x
  (Sec  f) <*> x = Sec  $ (<*> x) <$> f
  (Mul  f) <*> x = Mul  $ (<*> x) <$> f
  (CMul f) <*> x = CMul $ (((<*> x) <$>) *** ((<*> x) <$>)) <$> f

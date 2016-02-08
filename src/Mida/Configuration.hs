--
-- This module describes how to parse Unix-style configuration files.
--
-- Copyright © 2014–2016 Mark Karpov
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

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Mida.Configuration
  ( Params
  , parseConfig
  , lookupCfg )
where

import Control.Applicative
import Control.Monad
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text.Lazy (Text)
import Data.Map (Map)
import Numeric.Natural
import Text.Megaparsec
import Text.Megaparsec.Text.Lazy
import qualified Data.Map as M
import qualified Text.Megaparsec.Lexer as L

-- | Collection of configuration parameters. They are kept as 'String's and
-- then converted on request.

type Params = Map String String

class Parsable a where
  parseValue :: String -> Maybe a

instance Parsable String where
  parseValue = Just

instance Parsable Natural where
  parseValue = fmap fst . listToMaybe . reads

instance Parsable Bool where
  parseValue "true"  = Just True
  parseValue "false" = Just False
  parseValue _       = Nothing

-- | Lookup a value from configuration parameters. Type of result determines
-- how value will be interpreted.

lookupCfg :: Parsable a
  => Params            -- ^ Collection of configuration parameters
  -> String            -- ^ Name of parameter to lookup
  -> a                 -- ^ Fallback value
  -> a                 -- ^ Result
lookupCfg cfg v d = fromMaybe d $ M.lookup v cfg >>= parseValue

-- | Parse configuration file.

parseConfig :: String -> Text -> Either String Params
parseConfig file = either (Left . show) Right . parse pConfig file

pConfig :: Parser Params
pConfig = M.fromList <$> (sc *> many pItem <* eof)

pItem :: Parser (String, String)
pItem = (,) <$> pIdentifier <* pOperator "=" <*> (pString <|> pThing)

pIdentifier :: Parser String
pIdentifier = lexeme $ (:) <$> first <*> many other
  where first = letterChar   <|> char '_'
        other = alphaNumChar <|> char '_'

pOperator :: String -> Parser String
pOperator = lexeme . string

pString :: Parser String
pString = lexeme $ char '"' >> manyTill L.charLiteral (char '"')

pThing :: Parser String
pThing = lexeme (some alphaNumChar)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

sc :: Parser ()
sc = L.space (void spaceChar) (L.skipLineComment "#") empty

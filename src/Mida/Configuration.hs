-- -*- Mode: Haskell; -*-
--
-- This module describes how to parse Unix-style configuration files.
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

module Mida.Configuration
    ( Params
    , parseConfig
    , lookupCfg )
where

import Control.Applicative
import Control.Monad
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Map as M
import qualified Data.Text.Lazy as T

import Text.Megaparsec
import Text.Megaparsec.Text.Lazy
import qualified Text.Megaparsec.Lexer as L

type Params = M.Map String String

class Parsable a where
    parseValue :: String -> Maybe a

instance Parsable String where
    parseValue = Just

instance Parsable Int where
    parseValue = parseNum

instance Parsable Bool where
    parseValue "true"  = Just True
    parseValue "false" = Just False
    parseValue _       = Nothing

lookupCfg :: Parsable a => Params -> String -> a -> a
lookupCfg cfg v d = fromMaybe d $ M.lookup v cfg >>= parseValue

parseNum :: (Num a, Read a) => String -> Maybe a
parseNum = fmap fst . listToMaybe . reads

parseConfig :: String -> T.Text -> Either String Params
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
pThing = lexeme $ some alphaNumChar

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

sc :: Parser ()
sc = L.space (void spaceChar) (L.skipLineComment "#") empty

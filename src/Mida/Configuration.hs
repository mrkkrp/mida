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

import Data.Char (isSpace)
import Data.Functor.Identity
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Map as M
import qualified Data.Text.Lazy as T

import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.Text.Lazy
import qualified Text.Parsec.Token as Token

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

parseConfig :: String -> T.Text -> Either String Params
parseConfig file = either (Left . show) Right . parse pConfig file

pConfig :: Parser Params
pConfig = M.fromList <$> (whiteSpace *> many pItem <* eof)

pItem :: Parser (String, String)
pItem = (,) <$> identifier <* reservedOp "=" <*> (pString <|> pThing)

pThing :: Parser String
pThing = lexeme $ many (satisfy $ not . isSpace)

lookupCfg :: Parsable a => Params -> String -> a -> a
lookupCfg cfg v d = fromMaybe d $ M.lookup v cfg >>= parseValue

lang :: GenLanguageDef T.Text () Identity
lang = Token.LanguageDef
       { Token.commentStart    = ""
       , Token.commentEnd      = ""
       , Token.commentLine     = "#"
       , Token.nestedComments  = True
       , Token.identStart      = letter <|> char '_'
       , Token.identLetter     = alphaNum <|> char '_'
       , Token.opStart         = Token.opLetter lang
       , Token.opLetter        = oneOf "="
       , Token.reservedNames   = ["true", "false"]
       , Token.reservedOpNames = ["="]
       , Token.caseSensitive   = True }

lexer :: Token.GenTokenParser T.Text () Identity
lexer = Token.makeTokenParser lang

identifier :: Parser String
identifier = Token.identifier lexer

pString :: Parser String
pString = Token.stringLiteral lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

lexeme :: Parser a -> Parser a
lexeme = Token.lexeme lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

parseNum :: (Num a, Read a) => String -> Maybe a
parseNum = fmap fst . listToMaybe . reads

-- -*- Mode: Haskell; -*-
--
-- Config module helps parse Unix-style configuration files.
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

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Config
    ( Params
    , parseConfig
    , lookupCfg )
where

import Data.Char (isDigit, isSpace)
import qualified Data.Map as M

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

----------------------------------------------------------------------------
--                               Data Types                               --
----------------------------------------------------------------------------

type Params = M.Map String String

class Parsable a where
    parseValue :: String -> Maybe a

instance Parsable String where
    parseValue = Just

instance Parsable Int where
    parseValue x
        | null x        = Nothing
        | all isDigit x = Just $ read x
        | otherwise     = Nothing

instance Parsable Bool where
    parseValue "true"  = Just True
    parseValue "false" = Just False
    parseValue _       = Nothing

----------------------------------------------------------------------------
--                          Language and Lexemes                          --
----------------------------------------------------------------------------

lang :: LanguageDef st
lang = emptyDef { Token.commentLine     = "#"
                , Token.identStart      = letter
                , Token.identLetter     = alphaNum
                , Token.reservedOpNames = ["="]
                , Token.caseSensitive   = True }

lexer :: Token.TokenParser st
lexer = Token.makeTokenParser lang

identifier :: Parser String
identifier = Token.identifier lexer

pstring :: Parser String
pstring = Token.stringLiteral lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

----------------------------------------------------------------------------
--                                Parsing                                 --
----------------------------------------------------------------------------

parseConfig :: String -> String -> Either String Params
parseConfig file str =
    case parse pConfig file str of
      Right x -> Right x
      Left  x -> Left $ show x

pConfig :: Parser Params
pConfig = do
  whiteSpace
  items <- many pItem
  return $ M.fromList items

pItem :: Parser (String, String)
pItem = do
  var <- identifier
  reservedOp "="
  val <- try pstring <|> (many $ satisfy (not . isSpace))
  whiteSpace
  return (var, val)

lookupCfg :: Parsable a => Params -> String -> a -> a
lookupCfg cfg v d = maybe d id $ M.lookup v cfg >>= parseValue

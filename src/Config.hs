-- -*- Mode: Haskell; -*-
--
-- Config module helps parse Unix-styled configuration files.
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

module Config
    ( parseConfig
    , parseInt
    , lookupStr
    , lookupInt )
    where

import Data.Char (isDigit)
import qualified Data.Map as M
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

-- data types --

type Params = M.Map String String

-- language and lexemes --

lang = emptyDef { Token.commentLine     = "#"
                , Token.identStart      = letter
                , Token.identLetter     = alphaNum
                , Token.reservedOpNames = ["="]
                , Token.caseSensitive   = True }

lexer = Token.makeTokenParser lang

identifier = Token.identifier    lexer
natural    = Token.natural       lexer
pstring    = Token.stringLiteral lexer
reservedOp = Token.reservedOp    lexer
whiteSpace = Token.whiteSpace    lexer

-- parsing --

pConfig :: Parser Params
pConfig =
    do whiteSpace
       items <- many (try pVarInt <|> pVarString)
       return $ M.fromList items

pVarInt :: Parser (String, String)
pVarInt =
    do var <- identifier
       reservedOp "="
       x   <- natural
       return (var, show x)

pVarString :: Parser (String, String)
pVarString =
    do var <- identifier
       reservedOp "="
       x   <- pstring
       return (var, x)

parseConfig :: String -> String -> Either String Params
parseConfig file str =
    case parse pConfig file str of
      (Right x) -> Right x
      (Left  x) -> Left $ show x

-- miscellaneous functions --

parseInt :: String -> Int -> Int
parseInt s x
    | null s        = x
    | all isDigit s = read s :: Int
    | otherwise     = x

lookupStr :: Params -> String -> String -> String
lookupStr cfg v d =
    case M.lookup v cfg of
      (Just x) -> x
      Nothing  -> d

lookupInt :: Params -> String -> Int -> Int
lookupInt cfg v d =
    case M.lookup v cfg of
      (Just x) -> parseInt x d
      Nothing  -> d

-- -*- Mode: Haskell; -*-
--
-- This module describes how to build syntax tree from textual
-- representation of MIDA statements.
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

module Mida.Representation.Parser
    ( Statement (..)
    , probeMida
    , parseMida )
where

import Control.Applicative ((<$>), (<*>), (<*), (*>))
import Data.Char (isSpace)
import Data.Functor.Identity
import Data.List (isInfixOf, isSuffixOf)

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.String
import qualified Text.Parsec.Token as Token

import Mida.Language.SyntaxTree (SyntaxTree, Sel (..))
import qualified Mida.Representation.Base as B

data Statement
    = Definition String SyntaxTree
    | Exposition SyntaxTree
      deriving (Eq, Show)

probeMida :: String -> Bool
probeMida arg = not $ or ["," `isSuffixOf` s, f "[]", f "{}", f "<>", f "()"]
    where s       = reverse . dropWhile isSpace . reverse $ arg
          g x     = length $ filter (== x) s
          f [x,y] = ((&&) <$> (> 0) <*> (/= g y)) (g x)
          f _     = False

parseMida :: String -> String -> Either String [Statement]
parseMida file str =
    case parse parser file str of
      Right x -> if null x
                 then Left $ "\"" ++ file ++ "\":\ninvalid definition syntax"
                 else Right x
      Left  x -> Left $ show x
    where parser = if B.defOp `isInfixOf` str
                   then pSource
                   else pExposition

pSource :: Parser [Statement]
pSource = whiteSpace *> many pDefinition <* eof

pDefinition :: Parser Statement
pDefinition = Definition <$> identifier <* reservedOp B.defOp <*> pPrinciple

pExposition :: Parser [Statement]
pExposition = (return . Exposition) <$> (whiteSpace *> pPrinciple <* eof)

pPrinciple :: Parser SyntaxTree
pPrinciple = sepBy (pExpression <|> pElement) (optional comma)

pElement :: Parser Sel
pElement
    =  try pRange
   <|> pValue
   <|> try pReference
   <|> pSection
   <|> try pMulti
   <|> pCMulti
   <?> "element"

pRange :: Parser Sel
pRange = Range <$> pNatural <* reservedOp B.rangeOp <*> pNatural

pValue :: Parser Sel
pValue = Value <$> pNatural

pNatural :: Parser Int
pNatural = fromIntegral <$> natural <?> "literal value"

pReference :: Parser Sel
pReference = Reference <$> identifier <* notFollowedBy (reservedOp B.defOp)

pSection :: Parser Sel
pSection = Section <$> brackets pPrinciple

pMulti :: Parser Sel
pMulti = Multi <$> braces pPrinciple

pCMulti :: Parser Sel
pCMulti = CMulti <$> braces (many $ (,) <$> angles pPrinciple <*> pPrinciple)

pExpression :: Parser Sel
pExpression = buildExpressionParser optTable (parens pExpression <|> pElement)
              <?> "expression"

optTable :: [[Operator String () Data.Functor.Identity.Identity Sel]]
optTable =
    [[ Prefix (reservedOp B.reverseOp >> return Reverse ) ]
     , [ Infix (reservedOp B.productOp  >> return Product ) AssocLeft
       , Infix (reservedOp B.divisionOp >> return Division) AssocLeft
       , Infix (reservedOp B.sumOp      >> return Sum     ) AssocLeft
       , Infix (reservedOp B.diffOp     >> return Diff    ) AssocLeft
       , Infix (reservedOp B.loopOp     >> return Loop    ) AssocLeft
       , Infix (reservedOp B.rotationOp >> return Rotation) AssocLeft ]]

lang :: LanguageDef st
lang = emptyDef
       { Token.commentStart    = ""
       , Token.commentEnd      = ""
       , Token.commentLine     = B.commentLine
       , Token.nestedComments  = True
       , Token.identStart      = letter <|> char '_'
       , Token.identLetter     = alphaNum <|> char '_'
       , Token.reservedOpNames = langOps
       , Token.caseSensitive   = True }

langOps :: [String]
langOps =
    [ B.productOp
    , B.divisionOp
    , B.sumOp
    , B.diffOp
    , B.loopOp
    , B.rotationOp
    , B.reverseOp
    , B.rangeOp
    , B.defOp ]

lexer :: Token.TokenParser st
lexer = Token.makeTokenParser lang

angles :: Parser a -> Parser a
angles = Token.angles lexer

braces :: Parser a -> Parser a
braces = Token.braces lexer

brackets :: Parser a -> Parser a
brackets = Token.brackets lexer

comma :: Parser String
comma = Token.comma lexer

identifier :: Parser String
identifier = Token.identifier lexer

natural :: Parser Integer
natural = Token.natural lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

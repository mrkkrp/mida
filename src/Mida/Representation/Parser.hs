--
-- This module describes how to build syntax tree from textual
-- representation of MIDA statements.
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

module Mida.Representation.Parser
  ( probeMida
  , parseMida )
where

import Control.Applicative (empty)
import Control.Monad (void)
import Data.Text.Lazy (Text)
import Mida.Language.SyntaxTree
import Numeric.Natural
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.Text.Lazy
import qualified Data.List.NonEmpty       as NE
import qualified Data.Text.Lazy           as T
import qualified Mida.Representation.Base as B
import qualified Text.Megaparsec.Lexer    as L

-- | Test if given fragment of MIDA code is finished and self-contained.

probeMida :: Text -> Bool
probeMida txt = not $ or
  ["," `T.isSuffixOf` stripped
  , f ("[", "]")
  , f ("{", "}")
  , f ("<", ">")
  , f ("(", ")") ]
  where stripped = T.strip txt
        f (x, y) = (&&) <$> (> 0) <*> (/= g y) $ g x
        g x = T.count x stripped

-- | Entry point for MIDA parsing.

parseMida
  :: String            -- ^ Name of file
  -> Text              -- ^ Text to parse
  -> Either String [Statement] -- ^ Error message or parsed statements
parseMida file txt =
  case parse parser file txt of
    Right x -> if null x
      then Left ('\"' : file ++ "\":\ninvalid definition syntax")
      else Right x
    Left  e -> Left (parseErrorPretty e)
  where parser = if T.pack B.defOp `T.isInfixOf` txt
         then pSource
         else pure <$> pExposition

pSource :: Parser [Statement]
pSource = sc *> many pDefinition <* eof

pDefinition :: Parser Statement
pDefinition = Definition <$> pIdentifier <* pOperator B.defOp <*> pPrinciple

pExposition :: Parser Statement
pExposition = Exposition <$> (sc *> pPrinciple <* eof)

pIdentifier :: Parser String
pIdentifier = lexeme $ (:) <$> first <*> many other
  where first = letterChar   <|> char '_'
        other = alphaNumChar <|> char '_'

pOperator :: String -> Parser String
pOperator = lexeme . string

pPrinciple :: Parser SyntaxTree
pPrinciple = sepBy (pExpression <|> pElement) (optional comma)

pElement :: Parser Sel
pElement
  =   try pRange
  <|> pValue
  <|> try pReference
  <|> pSection
  <|> try pMulti
  <|> pCMulti
  <?> "element"

pRange :: Parser Sel
pRange = Range <$> pNatural <* pOperator B.rangeOp <*> pNatural

pValue :: Parser Sel
pValue = Value <$> pNatural

pNatural :: Parser Natural
pNatural = fromIntegral <$> natural <?> "literal value"

pReference :: Parser Sel
pReference = Reference <$> pIdentifier <* notFollowedBy (pOperator B.defOp)

pSection :: Parser Sel
pSection = Section <$> brackets pPrinciple

pMulti :: Parser Sel
pMulti = Multi <$> braces pPrinciple

pCMulti :: Parser Sel
pCMulti = CMulti . NE.fromList <$>
  braces (some $ (,) <$> angles pPrinciple <*> pPrinciple)

pExpression :: Parser Sel
pExpression = makeExprParser (parens pExpression <|> pElement) optTable

optTable :: [[Operator Parser Sel]]
optTable =
    [[ Prefix (pOperator B.reverseOp *> pure Reverse ) ]
     , [ InfixL (pOperator B.productOp  *> pure Product )
       , InfixL (pOperator B.divisionOp *> pure Division)
       , InfixL (pOperator B.sumOp      *> pure Sum     )
       , InfixL (pOperator B.diffOp     *> pure Diff    )
       , InfixL (pOperator B.loopOp     *> pure Loop    )
       , InfixL (pOperator B.rotationOp *> pure Rotation) ]]

angles :: Parser a -> Parser a
angles = between (symbol "<") (symbol ">")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

comma :: Parser ()
comma = void . hidden . symbol $ ","

natural :: Parser Integer
natural = lexeme L.integer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

symbol :: String -> Parser String
symbol = L.symbol sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

sc :: Parser ()
sc = L.space (void spaceChar) (L.skipLineComment "#") empty

{-
This file is part of Fronsduk.

Fronsduk is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Fronsduk is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Fronsduk.  If not, see <http://www.gnu.org/licenses/>.
-}

module Qzitche where

import Machine
import Bytecode
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (emptyDef)
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Put
import Data.Char

data SyntaxElement = Identifier String |
                     CharString String |
                     FuncCall SyntaxElement [SyntaxElement]
                   deriving(Show)

getPrimitiveCode :: String -> Control
getPrimitiveCode "print" = Ld § [0 :: Int, 0 :: Int] § Print § Rtn § []
getPrimitiveCode _ = []

lexer :: P.TokenParser ()
lexer = P.makeTokenParser
        (emptyDef { P.commentLine = ";"
                  , P.identStart = letter <|> char '_'
                  , P.identLetter = letter <|> digit <|> char '_'
                  })

whiteSpace = P.whiteSpace lexer
stringLiteral = P.stringLiteral lexer
identifier = P.identifier lexer
parens = P.parens lexer
commaSep = P.commaSep lexer

funcArgs :: Parser [SyntaxElement]
funcArgs = commaSep expr

expr :: Parser SyntaxElement
expr = whiteSpace >>=
       (\_ ->do{ s <- stringLiteral
               ; whiteSpace
               ; return $ CharString $ s
               }
             <|>
             do{ i <- identifier
               ; args <- parens funcArgs
               ; whiteSpace
               ; return $ FuncCall (Identifier i) args
               })

program :: Parser [SyntaxElement]
program = many1 expr

class Compilable a where
  toByteCode :: a -> Control

instance Compilable SyntaxElement where
  toByteCode (Identifier s) = getPrimitiveCode s
  toByteCode (CharString s) = Nil §
                              (concat $
                               map (\c -> Ldc § ord c § Cons § []) $ reverse s)
  toByteCode (FuncCall i args) = (Nil §
                                  (concat $
                                   map (\arg -> toByteCode arg ++ (Cons § [])) args)) ++
                                 (Ldf §
                                 toByteCode i § Ap § [])

instance (Compilable a) => Compilable [a] where
  toByteCode elts = concat $ map toByteCode elts

main :: IO ()
main = do
  input <- getContents
  let c = case (parse program "" input) of
        Left err -> error $ "Parsing failed : " ++ show err
        Right x -> x
    in BL.putStr $ runPut $ serializeControl $ toByteCode c

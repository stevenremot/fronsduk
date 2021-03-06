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

module Assembler where

import Machine
import Bytecode
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (emptyDef)
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Put

lexer :: P.TokenParser ()
lexer = P.makeTokenParser
        (emptyDef { P.commentLine = ";"
                  , P.caseSensitive = False
                  })

whiteSpace = P.whiteSpace lexer
natural = P.natural lexer
parens = P.parens lexer
-- reserved = P.reserved lexer
identifier = P.identifier lexer

control :: Parser Control
control = many1 expr

getOperator :: String -> Operator
getOperator "Ld" = Ld
getOperator "Ldc" = Ldc
getOperator "Ldf" = Ldf
getOperator "Nil" = Nil
getOperator "Dum" = Dum
getOperator "Ap" = Ap
getOperator "Rap" = Rap
getOperator "Rtn" = Rtn
getOperator "Cons" = Cons
getOperator "Car" = Car
getOperator "Cdr" = Cdr
getOperator "Sel" = Sel
getOperator "Join" = Join
getOperator "Plus" = Plus
getOperator "Minus" = Minus
getOperator "Times" = Times
getOperator "Divide" = Divide
getOperator "Eq" = Eq
getOperator "Cmp" = Cmp
getOperator "And" = And
getOperator "Or" = Or
getOperator "Not" = Not
getOperator "Print" = Print
getOperator "Read" = Read

expr :: Parser Value
expr = whiteSpace >>=
       (\_ -> (do{ i <- natural
                ; return $ NumberValue $ fromIntegral i
                }
               <|>
               (do{ s <- identifier
                  ; return $ OperatorValue $ getOperator s
                  })
               <|>
               (do { s <- parens control
                   ; return $ ListValue $ s
                  })))

main :: IO ()
main = do
  input <- getContents
  let c = case (parse control "" input) of
        Left err -> error $ "Parsing failed : " ++ show err
        Right x -> x
    in BL.putStr $ runPut $ serializeControl c

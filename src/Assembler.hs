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
getOperator "And" = And
getOperator "Or" = Or
getOperator "Not" = Not

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

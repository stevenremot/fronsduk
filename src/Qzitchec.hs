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

module Qzitchec where

import Machine
import Bytecode
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (emptyDef)
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Put
import Data.Char
import Data.Maybe
import Data.Map (Map, insertWith, findWithDefault, empty)
import qualified Control.Monad.State as MS

data SyntaxElement = Identifier String |
                     CharString String |
                     NumberElement Int |
                     FuncCall SyntaxElement [SyntaxElement] |
                     LetClause [(String, SyntaxElement)] [SyntaxElement] |
                     Condition SyntaxElement [SyntaxElement] [SyntaxElement]
                   deriving(Show)

binaryOperator :: Operator -> Control
binaryOperator op = Ld § [ 0 :: Int, 0 :: Int] § Ld § [0 :: Int, 1 :: Int] § op § Rtn § []

getPrimitiveCode :: String -> Control
getPrimitiveCode "print" = Ld § [0 :: Int, 0 :: Int] § Print § Rtn § []
getPrimitiveCode "read" = Read § Rtn § []

getPrimitiveCode "+" = binaryOperator Plus
getPrimitiveCode "-" = binaryOperator Minus
getPrimitiveCode "*" = binaryOperator Times
getPrimitiveCode "/" = binaryOperator Divide
getPrimitiveCode "=" = binaryOperator Eq
getPrimitiveCode "&&" = binaryOperator And
getPrimitiveCode "||" = binaryOperator Or
getPrimitiveCode "!" = Ld § [0 :: Int, 0 :: Int] § Not § Rtn § []

getPrimitiveCode i = error $ "No binding for identifier " ++ i

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
commaSep1 = P.commaSep1 lexer
natural = P.natural lexer

funcArgs :: Parser [SyntaxElement]
funcArgs = commaSep expr

letBinding :: Parser (String, SyntaxElement)
letBinding = whiteSpace >>=
             (\_ -> do{ s <- identifier
                      ; whiteSpace
                      ; string ":="
                      ; whiteSpace
                      ; e <- expr
                      ; whiteSpace
                      ; return $ (s, e)
                      })

letForm :: Parser SyntaxElement
letForm = do{ whiteSpace
            ; string "let"
            ; whiteSpace
            ; varBindings <- commaSep1 letBinding
            ; whiteSpace
            ; string "in"
            ; body <- program
            ; whiteSpace
            ; string "end"
            ; whiteSpace
            ; return $ LetClause varBindings body
            }

unit :: Parser SyntaxElement
unit = whiteSpace >>=
       (\_ ->do{ s <- stringLiteral
               ; return $ CharString $ s
               }
             <|>
             do{ i <- natural
               ; return $ NumberElement $ fromInteger i
               }
             <|>
             try (do{ l <- letForm
                    ; return l
                    })
             <|>
             try (do{ i <- identifier
                   ; args <- parens funcArgs
                   ; return $ FuncCall (Identifier i) args
                   })
             <|>
             try (do{ i <- identifier
                    ; if i `elem` ["in", "end", ",", "else", "then"]
                      then fail "End of expression"
                      else return $ Identifier i
                    }))
       >>= (\v -> whiteSpace >>= (\_ -> return v))

term :: Parser SyntaxElement
term = whiteSpace >>=
       (\_ -> try (do { s1 <- unit
                     ; whiteSpace
                     ; op <- string "*" <|> string "/"
                     ; whiteSpace
                     ; s2 <- term
                     ; return $ FuncCall (Identifier op) [s1, s2]
                     })
              <|> unit)
       >>= (\v -> whiteSpace >>= (\_ -> return v))

addition :: Parser SyntaxElement
addition = whiteSpace >>=
           (\_ -> try (do { s1 <- term
                          ; whiteSpace
                          ; op <- string "+" <|> string "-"
                          ; whiteSpace
                          ; s2 <- addition
                          ; return $ FuncCall (Identifier op) [s1, s2]
                          })
                  <|> term)
       >>= (\v -> whiteSpace >>= (\_ -> return v))

comparison :: Parser SyntaxElement
comparison = whiteSpace >>=
           (\_ -> try (do { s1 <- addition
                          ; whiteSpace
                          ; op <- string "="
                          ; whiteSpace
                          ; s2 <- comparison
                          ; return $ FuncCall (Identifier op) [s1, s2]
                          })
                  <|> addition)
           >>= (\v -> whiteSpace >>= (\_ -> return v))

logicalOp :: Parser SyntaxElement
logicalOp = whiteSpace >>=
       (\_ -> do { string "!"
                 ; s <- expr
                 ; return $ FuncCall (Identifier "!") [s]
                 }
              <|>
              try (do { s1 <- comparison
                     ; whiteSpace
                     ; op <- string "&&" <|> string "||"
                     ; whiteSpace
                     ; s2 <- expr
                     ; return $ FuncCall (Identifier op) [s1, s2]
                     })
              <|> comparison)
       >>= (\v -> whiteSpace >>= (\_ -> return v))

expr :: Parser SyntaxElement
expr = whiteSpace >>=
       (\_ -> do { string "if"
                 ; condition <- logicalOp
                 ; string "then"
                 ; ifTrue <- program
                 ; string "else"
                 ; ifFalse <- program
                 ; string "end"
                 ; return $ Condition condition ifTrue ifFalse
                 }
              <|>
              parens expr
              <|> logicalOp)
       >>= (\v -> whiteSpace >>= (\_ -> return v))

program :: Parser [SyntaxElement]
program = many1 expr


type Bindings = (Map String [(Int, Int)])

data CompilationState = CompilationState{ bindings :: Bindings,
                                          depth :: Int }

emptyState = CompilationState empty 0

putBinding :: CompilationState -> String -> (Int, Int) -> CompilationState
putBinding cs i pos =
  CompilationState (insertWith (++) i [pos] $ bindings cs) $ depth cs

getBinding :: CompilationState -> String -> Maybe (Int, Int)
getBinding cs i = let v = findWithDefault [] i $ bindings cs
                  in if null v
                     then Nothing
                     else Just $ head v

popBinding :: CompilationState -> String -> CompilationState
popBinding cs i =
  CompilationState (insertWith (\_ a -> tail a) i [] $ bindings cs) $ depth cs

incDepth :: CompilationState -> CompilationState
incDepth cs = CompilationState (bindings cs) $ depth cs + 1

decDepth :: CompilationState -> CompilationState
decDepth cs = CompilationState (bindings cs) $ max 0 $ depth cs - 1

type CompilationUnit = MS.State CompilationState Control

class Compilable a where
  compileToByteCode :: a -> CompilationUnit

instance Compilable SyntaxElement where
  compileToByteCode (Identifier s) = do
    state <- MS.get
    case getBinding state s of
      Nothing -> return $ getPrimitiveCode s
      Just (row, col) -> return $ Ld § [depth state - row, col] § []

  compileToByteCode (CharString s) = return $ Nil §
                                     (concat $
                                      map (\c -> Ldc § ord c § Cons § []) $
                                      reverse s)
  compileToByteCode (FuncCall i args) = do
    compiledArgs <- compileArgs args
    compiledFunc <- compileToByteCode i
    return $ (Nil § compiledArgs) ++
      (Ldf § compiledFunc § []) ++ (Ap § [])

  compileToByteCode (NumberElement i) = return $ Ldc § i § []
  compileToByteCode (LetClause varBindings body) =
    let names = map fst varBindings
    in do
      state <- MS.get
      compiledBindings <- compileBindings varBindings 0
      newState <- registerBindings names 0
      MS.put newState
      compiledBody <- compileToByteCode body
      MS.put state
      return $ compiledBindings ++ (Ldf § (compiledBody ++ (Rtn § [])) § Ap § [])

  compileToByteCode (Condition condition ifTrue ifFalse) = do
    compiledCondition <- compileToByteCode condition
    compiledIfTrue <- compileToByteCode ifTrue
    compiledIfFalse <- compileToByteCode ifFalse
    return $ compiledCondition ++ (Sel § []) ++
      ((compiledIfTrue ++ (Join § [])) §
       (compiledIfFalse ++ (Join § [])) § [])

registerBindings :: [String] -> Int -> MS.State CompilationState CompilationState
registerBindings [] _ = MS.get >>= return.incDepth >>= return
registerBindings (ident : idents) pos = do
  state <- registerBindings idents $ pos + 1
  return $ putBinding state ident (depth state, pos)

compileBindings :: [(String, SyntaxElement)] -> Int -> CompilationUnit
compileBindings [] _ = return $ Nil § []
compileBindings (bdg : bdgs) pos =
  let (identifier, value) = bdg
  in do
    state <- MS.get
    MS.put $ putBinding state identifier (depth state + 1, pos)
    compiledValue <- compileToByteCode value
    compiledBindings <- compileBindings bdgs (pos + 1)
    return $ compiledBindings ++ (compiledValue ++ (Cons § []))

compileArgs :: [SyntaxElement] -> CompilationUnit
compileArgs [] = return []
compileArgs (arg : args) = do
  headArg <- compileToByteCode arg
  tailArgs <- compileArgs args
  return $ headArg ++ (Cons § []) ++ tailArgs

instance (Compilable a) => Compilable [a] where
  compileToByteCode [] = return []
  compileToByteCode (e : elts) = do
    compiledElt <- compileToByteCode e
    compiledElts <- compileToByteCode elts
    return $ compiledElt ++ compiledElts

parseQzitche :: String -> [SyntaxElement]
parseQzitche input = case (parse program "" input) of
                           Left err -> error $ "Parsing failed : " ++ show err
                           Right x -> x

compileSyntax :: [SyntaxElement] -> Control
compileSyntax code =
  fst $ MS.runState (compileToByteCode code) $ CompilationState empty 0

compile :: String -> Control
compile = compileSyntax.parseQzitche

main :: IO ()
main = do
  input <- getContents
  BL.putStr $ runPut $ serializeControl $ compile input

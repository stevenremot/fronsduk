module Qzitche.Parse where


import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (emptyDef)

data SyntaxElement = Identifier String |
                     CharString String |
                     NumberElement Int |
                     FuncCall SyntaxElement [SyntaxElement] |
                     LetClause [(String, SyntaxElement)] [SyntaxElement] |
                     Condition SyntaxElement [SyntaxElement] [SyntaxElement] |
                     AnonymousFunc [String] [SyntaxElement] |
                     FuncDef String [String] [SyntaxElement] |
                     ListElement [SyntaxElement]
                   deriving(Show)

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
squares = P.squares lexer

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

funcArgsList :: Parser [String]
funcArgsList = parens $ commaSep identifier

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
                          ; op <- string "=" <|>
                                  try (string ">=") <|> string ">" <|>
                                  try (string "<=") <|> string "<"
                          ; whiteSpace
                          ; s2 <- comparison
                          ; return $ FuncCall (Identifier op) [s1, s2]
                          })
                  <|> addition)
           >>= (\v -> whiteSpace >>= (\_ -> return v))

notOp :: Parser SyntaxElement
notOp = whiteSpace >>=
        (\_ -> do { string "!"
                  ; s <- comparison <|> parens expr
                  ; return $ FuncCall (Identifier "!") [s]
                  }
               <|> comparison)
        >>= (\v -> whiteSpace >>= (\_ -> return v))

logicalOp :: Parser SyntaxElement
logicalOp = whiteSpace >>=
       (\_ -> try (do { s1 <- notOp
                     ; whiteSpace
                     ; op <- string "&&" <|> string "||"
                     ; whiteSpace
                     ; s2 <- expr
                     ; return $ FuncCall (Identifier op) [s1, s2]
                     })
              <|> notOp)
       >>= (\v -> whiteSpace >>= (\_ -> return v))

expr :: Parser SyntaxElement
expr = whiteSpace >>=
       (\_ -> try (do { string "if"
                      ; condition <- expr
                      ; string "then"
                      ; ifTrue <- program
                      ; string "else"
                      ; ifFalse <- program
                      ; string "end"
                      ; return $ Condition condition ifTrue ifFalse
                      })
              <|>
              try (do { string "fn"
                      ; whiteSpace
                      ; args <- funcArgsList
                      ; body <- program
                      ; string "end"
                      ; return $ AnonymousFunc args body
                      })
              <|>
              do { elts <- squares $ commaSep expr
                 ; return $ ListElement elts
                 }
              <|>
              try (do{ l <- letForm
                     ; return l
                     })
              <|>
              parens expr
              <|> logicalOp)
       >>= (\v -> whiteSpace >>= (\_ -> return v))

topExpr :: Parser SyntaxElement
topExpr = whiteSpace >>=
          (\_ -> try (do { string "fn"
                         ; whiteSpace
                         ; name <- identifier
                         ; whiteSpace
                         ; args <- funcArgsList
                         ; body <- program
                         ; string "end"
                         ; return $ FuncDef name args body
                         })
                 <|> expr)
          >>= (\v -> whiteSpace >>= (\_ -> return v))

program :: Parser [SyntaxElement]
program = many1 topExpr

sanitizeError :: ParseError -> String
sanitizeError err =
  map repl $ show err
  where repl '\n' = ' '
        repl x = x

parseQzitche :: String -> [SyntaxElement]
parseQzitche input = case (parse program "" input) of
                           Left err -> error $ "Parsing failed : " ++ sanitizeError err
                           Right x -> x

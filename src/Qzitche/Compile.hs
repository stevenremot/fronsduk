module Qzitche.Compile where

import Machine
import Qzitche.Parse
import Data.Char
import Data.Maybe()
import Data.List
import Data.Map (Map, insertWith, findWithDefault, empty)
import qualified Control.Monad.State as MS

binaryOperator :: Operator -> Control
binaryOperator op = Ldf § (Ld § [ 0 :: Int, 1 :: Int] § Ld § [0 :: Int, 0 :: Int] § op § Rtn § []) § []

getPrimitiveCode :: String -> Control
getPrimitiveCode "print" = Ldf § (Ld § [0 :: Int, 0 :: Int] § Print § Rtn § []) § []
getPrimitiveCode "read" = Ldf § (Read § Rtn § []) § []

getPrimitiveCode "+" = binaryOperator Plus
getPrimitiveCode "-" = binaryOperator Minus
getPrimitiveCode "*" = binaryOperator Times
getPrimitiveCode "/" = binaryOperator Divide
getPrimitiveCode "=" = binaryOperator Eq
getPrimitiveCode ">" = binaryOperator Cmp
getPrimitiveCode "<" =
  Ldf § (
    Ld § [0 :: Int, 1 :: Int] § Ld § [0 :: Int, 0 :: Int] § Cmp §
    Ld § [0 :: Int, 1 :: Int] § Ld § [0 :: Int, 0 :: Int] § Eq §
    Or § Not § Rtn § []) § []
getPrimitiveCode ">=" =
  Ldf § (
    Ld § [0 :: Int, 1 :: Int] § Ld § [0 :: Int, 0 :: Int] § Cmp §
    Ld § [0 :: Int, 1 :: Int] § Ld § [0 :: Int, 0 :: Int] § Eq §
    Or § Rtn § []) § []
getPrimitiveCode "<=" = Ldf § (Ld § [ 0 :: Int, 1 :: Int] § Ld § [0 :: Int, 0 :: Int] § Cmp § Not § Rtn § []) § []
getPrimitiveCode "&&" = binaryOperator And
getPrimitiveCode "||" = binaryOperator Or
getPrimitiveCode "!" = Ldf § (Ld § [0 :: Int, 0 :: Int] § Not § Rtn § []) § []

getPrimitiveCode "head" = Ldf § (Ld § [0 :: Int, 0 :: Int] § Car § Rtn § []) § []
getPrimitiveCode "tail" = Ldf § (Ld § [0 :: Int, 0 :: Int] § Cdr § Rtn § []) § []
getPrimitiveCode "cons" = Ldf § (Ld § [0 :: Int, 1 :: Int] § Ld § [0 :: Int, 0 :: Int] § Cons § Rtn § []) § []

getPrimitiveCode i = error $ "No binding for identifier " ++ i

type Bindings = (Map String [(Int, Int)])

data CompilationState = CompilationState{ bindings :: Bindings,
                                          depth :: Int }

emptyState :: CompilationState
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

type CompilationUnit = MS.State CompilationState Control

class Compilable a where
  compileToByteCode :: a -> CompilationUnit
  isTopLevel :: a -> Bool
  getIdentifier :: a -> String

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
    return $ compiledArgs ++
      compiledFunc ++ (Ap § [])

  compileToByteCode (NumberElement i) = return $ Ldc § i § []
  compileToByteCode (LetClause varBindings body) =
    let names = map fst varBindings
    in do
      state <- MS.get
      compiledBindings <- compileBindings varBindings 0
      MS.put $ incDepth state
      newState <- registerBindings names 0
      MS.put $ newState
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

  compileToByteCode (AnonymousFunc args body) = compileFunc args body
  compileToByteCode (FuncDef _ args body) = compileFunc args body

  compileToByteCode (ListElement []) = return $ Nil § []
  compileToByteCode (ListElement (elt : elts)) = do
    compiledElt <- compileToByteCode elt
    compiledElts <- compileToByteCode $ ListElement elts
    return $ compiledElts ++ (compiledElt ++ (Cons § []))

  isTopLevel (FuncDef _ _ _) = True
  isTopLevel _ = False

  getIdentifier (FuncDef name _ _) = name
  getIdentifier _ = error "No top-level definition"

registerBindings :: [String] -> Int -> MS.State CompilationState CompilationState
registerBindings [] _ = MS.get >>= return
registerBindings (ident : idents) pos = do
  state <- registerBindings idents $ pos + 1
  return $ putBinding state ident (depth state, pos)

compileBindings :: (Compilable a) => [(String, a)] -> Int -> CompilationUnit
compileBindings [] _ = return $ Nil § []
compileBindings (bdg : bdgs) pos =
  let (identifier, value) = bdg
  in do
    compiledValue <- compileToByteCode value
    compiledBindings <- compileBindings bdgs (pos + 1)
    return $ compiledBindings ++ (compiledValue ++ (Cons § []))

compileArgs :: [SyntaxElement] -> CompilationUnit
compileArgs [] = return $ Nil § []
compileArgs (arg : args) = do
  headArg <- compileToByteCode arg
  tailArgs <- compileArgs args
  return $ tailArgs ++ (headArg ++ (Cons § []))

compileFunc :: [String] -> [SyntaxElement] -> CompilationUnit
compileFunc args body = do
  state <- MS.get
  MS.put $ incDepth state
  newState <- registerBindings args 0
  MS.put newState
  compiledBody <- compileToByteCode body
  MS.put state
  return $ Ldf § (compiledBody ++ (Rtn § [])) § []

compileBody :: (Compilable a) => [a] -> CompilationUnit
compileBody [] = return []
compileBody (e : elts) = do
    compiledElt <- compileToByteCode e
    compiledElts <- compileToByteCode elts
    return $ compiledElt ++ compiledElts

instance (Compilable a) => Compilable [a] where
  -- compileToByteCode = compileBody
  compileToByteCode exprs =
    let (defs, other) = partition isTopLevel exprs
    in if null defs
       then compileBody other
       else let names = map getIdentifier defs
            in let defBindings = map (\d -> (getIdentifier d, d)) defs
               in do
                 state <- MS.get
                 newState <- registerBindings names 0
                 MS.put newState
                 compiledBindings <- compileBindings defBindings 0
                 compiledBody <- compileBody other
                 MS.put state
                 return $ Dum § compiledBindings ++
                   (Ldf § (compiledBody ++ (Rtn § [])) § Rap § [])

  isTopLevel = const False
  getIdentifier = const ""

compileSyntax :: [SyntaxElement] -> Control
compileSyntax code =
  fst $ MS.runState (compileToByteCode code) emptyState

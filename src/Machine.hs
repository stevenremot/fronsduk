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

module Machine where

import Data.Char

-- Machine base operators
data Operator = Ld | Ldc | Ldf |
                Nil | Dum |
                Ap | Rap | Rtn |
                Cons | Car | Cdr |
                Sel | Join |
                Plus | Minus | Times | Divide |
                Eq | And | Or | Not |
                Print | Read
              deriving (Show, Eq)

-- A value in the machine
data Value = OperatorValue Operator |
             NumberValue Int |
             -- TODO: More SECD-ish list implementation, with cons and nil ?
             ListValue [Value] |
             -- TODO: Find something more elegant, as the second element is
             -- an environment
             ClosureValue [Value] [[Value]]

instance Show Value where
  show (OperatorValue op) = show op
  show (NumberValue i) = show i
  show (ListValue l) = show l
  -- Do not show environment to avoid recursion...
  show (ClosureValue f _) = "(closure " ++ show f ++ ")"

class Valuable a where
  toValue :: a -> Value

instance Valuable Value where
  toValue = id

instance Valuable Operator where
  toValue = OperatorValue

instance Valuable Int where
  toValue = NumberValue

instance (Valuable a) => Valuable [a] where
  toValue = ListValue . (map toValue)

isTrue :: Value -> Bool
isTrue (NumberValue 0) = False
isTrue (NumberValue _) = True
isTrue (ListValue []) = False
isTrue (ListValue (_ : _)) = True
isTrue (OperatorValue _) = error "Operator is not like a boolean."
isTrue (ClosureValue _ _) = error "Closure is not like a boolean."

-- Registers

-- The stack contains current calculations
type Stack = [Value]

-- The environment contains the lexical bindings
type Environment = [[Value]]

-- The control contains the running program
type Control = [Value]

-- Utility operator to add a value to a value stack
(§) :: (Valuable a) => a -> [Value] -> [Value]
a § c = (toValue a) : c

infixr §

-- The dump is a temporary push of the other registers
data Dump = Dump { dumpS :: Stack,
                   dumpE :: Environment,
                   dumpC :: Control }
            deriving (Show)

-- Contains the current state of the virtual machine
data Registers = Registers { regS :: Stack,
                             regE :: Environment,
                             regC :: Control,
                             regD :: [Dump] }

instance Show Registers where
  show (Registers s e c d) = "S : " ++ show s ++
                             "\nE : " ++ show e ++
                             "\nC : " ++ show c ++
                             "\nD : " ++ show d

-- Utility to create a recursive environment for recursivity
recursiveEnv :: [Value] -> [Value]
recursiveEnv env = map (\(ClosureValue f (_ : env')) -> ClosureValue f $
                                                      recursiveEnv env : env') env

applyNumBinOperator :: (Int -> Int -> Int) -> Registers -> IO Registers
applyNumBinOperator op (Registers ((NumberValue a) : (NumberValue b) : s) e c d) =
  return  $ Registers (op a b § s) e c d

applyNumBinOperator _ _ = error "No matching for call binary operator with register."

-- Recursive operators
replaceDummy :: [Value] -> [Value] -> [Value]
replaceDummy fs e = map (\(ClosureValue f (_ : e')) -> ClosureValue f (e : e')) fs

-- Applies an operator to the current registers
applyOperator :: Operator -> Registers -> IO Registers
applyOperator Ld (Registers
                  s
                  e
                  ((ListValue ((NumberValue i) : (NumberValue j) : [])) : c)
                  d) =
  return $ Registers ((e !! i) !! j : s) e c d

applyOperator Ldc (Registers s e (v : c) d) =
  return $ Registers (v : s) e c d

applyOperator Ldf (Registers s e ((ListValue f) : c) d) =
  return  $ Registers ((ClosureValue f e) : s) e c d

applyOperator Nil (Registers s e c d) =
  return  $ Registers (ListValue [] : s) e c d

applyOperator Dum (Registers s e c d) =
  return  $ Registers s ([] : e) c d

applyOperator Ap (Registers ((ClosureValue f e') : (ListValue args) : s) e c d) =
  return  $ Registers [] (args : e') f (Dump s e c : d)

applyOperator Rap (Registers ((ClosureValue f (_ : e')) : (ListValue args) : s) (_ : e) c d) =
  return  $ (Registers [] ((replaceDummy args (recursiveEnv args)) : e') f ((Dump s e c) : d))

applyOperator Rtn (Registers (x : _) _ _ ((Dump s' e' c') : d)) =
  return  $ Registers (x : s') e' c' d

applyOperator Cons (Registers (v : ListValue l : s) e c d) =
  return  $ Registers (ListValue (v : l) : s) e c d

applyOperator Car (Registers (ListValue (x : _) : s) e c d) =
  return  $ Registers (x : s) e c d

applyOperator Cdr (Registers (ListValue (_ : xs) : s) e c d) =
  return  $ Registers (ListValue xs : s) e c d

applyOperator Sel (Registers (cond : s) e (ListValue whenTrue : ListValue whenFalse : c) d)
  | isTrue cond = return  $ Registers s e whenTrue ((Dump s e c) : d)
  | otherwise = return  $ Registers s e whenFalse ((Dump s e c) : d)

applyOperator Join (Registers s e _ ((Dump _ _ c') : d)) =
  return  $ Registers s e c' d

applyOperator Plus r = applyNumBinOperator (+) r
applyOperator Minus r  = applyNumBinOperator (-) r
applyOperator Times r = applyNumBinOperator (*) r
applyOperator Divide r = applyNumBinOperator quot r

applyOperator Eq r = applyNumBinOperator (\a b ->  if (a == b) then 1 else 0) r
applyOperator And (Registers (v1 : v2 : s) e c d) =
  return  $ Registers ((if isTrue v1 && isTrue v2 then 1 :: Int else 0 :: Int) § s) e c d
applyOperator Or (Registers (v1 : v2 : s) e c d) =
  return  $ Registers ((if isTrue v1 || isTrue v2 then 1 :: Int else 0 :: Int) § s) e c d
applyOperator Not (Registers (v : s) e c d) =
  return  $ Registers ((if isTrue v then 0 :: Int else 1 :: Int) § s) e c d

applyOperator Print (Registers ((ListValue v) : s) e c d) = do
  putStr $ map (\(NumberValue i) -> chr i) v
  return $ Registers (ListValue v : s) e c d

applyOperator Read (Registers s e c d) = do
  ch <- getChar
  return $ Registers (ord ch § s) e c d

applyOperator op reg = error $ "Op : " ++ (show op) ++ ", Register : " ++ (show reg)

-- Do a step
doStep :: Registers -> IO Registers
doStep (Registers s e [] []) = return  $ Registers s e [] []
doStep (Registers _ _ [] ((Dump s' e' c') : d)) =
  return  $ Registers s' e' c' d
doStep (Registers s e (OperatorValue op : c) d) =
  applyOperator op (Registers s e c d)
doStep r = error $ "Non operator : " ++ show r


-- Running the whole machine
runMachine :: Registers -> IO Registers
runMachine (Registers s e [] []) = return  $ Registers s e [] []
runMachine r = (doStep r) >>= runMachine

-- Running the code
runControl :: Control -> IO (Maybe Value)
runControl c = do
  r <- runMachine $ Registers [] [] c []
  let (Registers s _ _ _) = r
    in if null s
       then return Nothing
       else return $ Just $ head s

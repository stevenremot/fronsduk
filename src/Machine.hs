module Machine where

-- Machine base operators
data Operator = Ld | Ldc | Ldf |
                Nil | Dum |
                Ap | Rap | Rtn |
                Cons | Car | Cdr |
                Sel | Join |
                Plus | Minus | Times | Divide |
                Eq | And | Or | Not
              deriving (Show)

-- A value in the machine
data Value = OperatorValue Operator |
             NumberValue Int |
             -- TODO: More SECD-ish list implementation, with cons and nil ?
             ListValue [Value] |
             -- TODO: Find something more elegant, as the second element is
             -- an environment
             ClosureValue [Value] [[Value]]
           deriving (Show)

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
               deriving (Show)

applyNumBinOperator :: (Int -> Int -> Int) -> Registers -> Registers
applyNumBinOperator op (Registers ((NumberValue a) : (NumberValue b) : s) e c d) =
  Registers (op a b § s) e c d

applyNumBinOperator _ _ = error "No matching for call binary operator with register."

-- Applies an operator to the current registers
applyOperator :: Operator -> Registers -> Registers
applyOperator Ld (Registers
                  s
                  e
                  ((ListValue ((NumberValue i) : (NumberValue j) : [])) : c)
                  d) =
  Registers ((e !! i) !! j : s) e c d

applyOperator Ldc (Registers s e (v : c) d) =
  Registers (v : s) e c d

applyOperator Ldf (Registers s e ((ListValue f) : c) d) =
  Registers ((ClosureValue f e) : s) e c d

applyOperator Nil (Registers s e c d) =
  Registers (ListValue [] : s) e c d

applyOperator Dum (Registers s e c d) =
  Registers s ([] : e) c d

applyOperator Ap (Registers ((ListValue args) : (ClosureValue f e') : s) e c d) =
  Registers [] (args : e') f (Dump s e c : d)

applyOperator Rap _  = error "Rap not implemented yet."

applyOperator Rtn (Registers (x : _) _ _ ((Dump s' e' c') : d)) =
  Registers (x : s') e' c' d

applyOperator Cons (Registers (ListValue l : s) e (v : c) d) =
  Registers (ListValue (v : l) : s) e c d

applyOperator Car (Registers (ListValue (x : _) : s) e c d) =
  Registers (x : s) e c d

applyOperator Cdr (Registers (ListValue (_ : xs) : s) e c d) =
  Registers (ListValue xs : s) e c d

applyOperator Sel (Registers (cond : s) e (ListValue whenTrue : ListValue whenFalse : c) d)
  | isTrue cond = Registers s e whenTrue ((Dump s e c) : d)
  | otherwise = Registers s e whenFalse ((Dump s e c) : d)

applyOperator Join (Registers s e _ ((Dump _ _ c') : d)) =
  Registers s e c' d

applyOperator Plus r = applyNumBinOperator (+) r
applyOperator Minus r  = applyNumBinOperator (-) r
applyOperator Times r = applyNumBinOperator (*) r
applyOperator Divide r = applyNumBinOperator quot r

applyOperator Eq r = applyNumBinOperator (\a b ->  if (a == b) then 1 else 0) r
applyOperator And (Registers (v1 : v2 : s) e c d) =
  Registers ((if isTrue v1 && isTrue v2 then 1 :: Int else 0 :: Int) § s) e c d
applyOperator Or (Registers (v1 : v2 : s) e c d) =
  Registers ((if isTrue v1 || isTrue v2 then 1 :: Int else 0 :: Int) § s) e c d
applyOperator Not (Registers (v : s) e c d) =
  Registers ((if isTrue v then 0 :: Int else 1 :: Int) § s) e c d

applyOperator op reg = error $ "Op : " ++ (show op) ++ ", Register : " ++ (show reg)

-- Running the machine
runMachine :: Registers -> Registers
runMachine (Registers s e [] []) = Registers s e [] []
runMachine (Registers _ _ [] ((Dump s' e' c') : d)) =
  runMachine $ Registers s' e' c' d
runMachine (Registers s e (OperatorValue op : c) d) =
  runMachine $ applyOperator op (Registers s e c d)


-- Running the code
runControl :: Control -> Maybe Value
runControl c = let (Registers s _ _ _) = runMachine $ Registers [] [] c []
               in if null s
                  then Nothing
                  else Just $ head s

-- Temp test
runMinusTest :: Maybe Value
runMinusTest = let minus = Ld § [0 :: Int, 1 :: Int] §
                           Ld § [0 :: Int, 0 :: Int] §
                           Minus § Rtn § []
               in runControl $
                  Ldf § minus §
                  Nil §
                  Cons § (5 :: Int) §
                  Cons § (7 :: Int) §
                  Ap § []

runCondTest :: Maybe Value
runCondTest = let whenTrue = Ldc § (1 :: Int) § Join § []
              in let whenFalse = Ldc § (2 :: Int) § Join § []
                 in runControl $
                    Ldc § (1 :: Int) §
                    Ldc § (2 :: Int) §
                    Plus §
                    Ldc § (3 :: Int) §
                    Eq § Not §
                    Sel §
                    whenTrue §
                    whenFalse § []

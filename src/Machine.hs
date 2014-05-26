module Machine where

-- Machine base operators
data Operator = Ld | Ldc | Ldf |
                Nil | Dum |
                Ap | Rap | Rtn |
                Cons | Car | Cdr |
                Sel | Join |
                Plus | Minus | Times | Divide
              deriving (Show)

-- A value in the machine
data Value = OperatorValue Operator |
             NumberValue Int |
             ListValue [Value] | -- TODO: More SECD-ish list implementation
             ClosureValue [Value] [[Value]] -- TODO: find something more elegant
           deriving (Show)

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

-- The dump is a temporary push of the other registers
data Dump = Dump { dumpS :: Stack,
                   dumpE :: Environment,
                   dumpC :: Control }
            deriving (Show)

data Registers = Registers { regS :: Stack,
                             regE :: Environment,
                             regC :: Control,
                             regD :: [Dump] }
                 deriving (Show)

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

applyOperator Plus (Registers (NumberValue a : NumberValue b : s) e c d) =
  Registers (NumberValue (a + b) : s) e c d

applyOperator Minus (Registers (NumberValue a : NumberValue b : s) e c d) =
  Registers (NumberValue (a - b) : s) e c d

applyOperator Times (Registers (NumberValue a : NumberValue b : s) e c d) =
  Registers (NumberValue (a * b) : s) e c d

applyOperator Divide (Registers (NumberValue a : NumberValue b : s) e c d) =
  Registers (NumberValue (quot a b) : s) e c d

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
runTest :: Maybe Value
runTest = runControl [(OperatorValue Nil),
                      (OperatorValue Dum),
                      (OperatorValue Ldf),
                      (ListValue [(OperatorValue Ld), (ListValue [(NumberValue 0),
                                                                  (NumberValue 1)]),
                                  (OperatorValue Ld), (ListValue [(NumberValue 0),
                                                                  (NumberValue 0)]),
                                  (OperatorValue Minus),
                                  (OperatorValue Rtn)]),
                      (OperatorValue Nil),
                      (OperatorValue Cons),
                      (NumberValue 5),
                      (OperatorValue Cons),
                      (NumberValue 7),
                      (OperatorValue Ap)]

module Bytecode where

import Data.Binary.Put
import Data.Binary.Get
import Data.Maybe
import Data.List

import Machine

opCodes :: [(Operator, Integer)]
opCodes =
  [(Ld, 1),
   (Ldc, 2),
   (Ldf, 3),
   (Nil, 4),
   (Dum, 5),
   (Ap, 6),
   (Rap, 7),
   (Rtn, 8),
   (Cons, 9),
   (Car, 10),
   (Cdr, 11),
   (Sel, 12),
   (Join, 13),
   (Plus, 14),
   (Minus, 15),
   (Times, 16),
   (Divide, 17),
   (Eq, 18),
   (And, 19),
   (Or, 20),
   (Not, 21),
   (Print, 22)]

getOpCode :: Operator -> Integer
getOpCode = fromJust . (flip lookup opCodes)

getOpFromCode :: Integer -> Operator
getOpFromCode n = fst $ fromJust $ (find (\(_, n') -> n' == n) opCodes)

--------------------------------------------------
-- Serilization
--------------------------------------------------

serializeValue :: Value -> Put
serializeValue (NumberValue i) = do
  putWord32be $ fromIntegral i
serializeValue (OperatorValue op) = do
  putWord8 $ fromIntegral $ getOpCode op
serializeValue (ListValue l) =
  foldl
  (\m v -> m >>= (\_ -> serializeValue v))
  (putWord32be $ fromIntegral $ length l)
  l
serializeValue v = error $ "Cannot convert " ++ show v ++ " to bytecode."

serializeControl :: Control -> Put
serializeControl (val : vs) =
  foldl
  (\m v -> m >>= (\_ -> serializeValue v))
  (serializeValue val)
  vs

--------------------------------------------------
-- Deserialization
--------------------------------------------------


deserializeOperator :: Get (Operator)
deserializeOperator = do
  w <- getWord8
  return $ getOpFromCode $ fromIntegral w

deserializeNumber :: Get (Int)
deserializeNumber = do
  w <- getWord32be
  return $ fromIntegral w

deserializeControlStep :: Get (Control)
deserializeControlStep = do
  operator <- deserializeOperator
  chain <- handleOperator operator
  return $ OperatorValue operator : chain

deserializeLimitedControlItem :: Int -> Int -> Get (Control)
deserializeLimitedControlItem n l
  | n == l = return []
  | otherwise = do
    step <- deserializeControlStep
    rest <- deserializeLimitedControlItem (n + (fromIntegral $ length step)) l
    return $ step ++ rest

deserializeLimitedControl :: Get (Control)
deserializeLimitedControl = do
  controlLength <- deserializeNumber
  chain <- deserializeLimitedControlItem 0 controlLength
  return [ListValue chain]

deserializePair :: Get (Value)
deserializePair = do
  controlLength <- deserializeNumber
  if controlLength /= 2
    then fail $ "Expecting a pair, but got a list of length " ++ show controlLength
    else do
    n1 <- deserializeNumber
    n2 <- deserializeNumber
    return $ ListValue [NumberValue n1, NumberValue n2]

encloseInList :: a -> Get [a]
encloseInList a = return [a]

handleOperator :: Operator -> Get (Control)
handleOperator Ld = deserializePair >>= encloseInList
handleOperator Ldc = deserializeNumber >>= (\n -> return [NumberValue n])
handleOperator Ldf = deserializeLimitedControl
handleOperator Sel = do
  true <- deserializeLimitedControl
  false <- deserializeLimitedControl
  return $ true ++ false
handleOperator _ = return []


deserializeControl :: Get (Control)
deserializeControl = do
  empty <- isEmpty
  if empty
    then return []
    else do
    step <- deserializeControlStep
    rest <- deserializeControl
    return $ step ++ rest

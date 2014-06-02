module Bytecode where

import qualified Data.ByteString.Lazy as BL
import Data.Binary.Put
import Data.Maybe

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
   (Not, 21)]

getOpCode :: Operator -> Integer
getOpCode = fromJust . (flip lookup opCodes)

serializeValue :: Value -> Put
serializeValue (NumberValue i) = do
  putWord8 $ fromIntegral i
serializeValue (OperatorValue op) = do
  putWord8 $ fromIntegral $ getOpCode op
serializeValue (ListValue l) =
  foldl
  (\m v -> m >>= (\_ -> serializeValue v))
  (putWord8 $ fromIntegral $ length l)
  l
serializeValue v = error $ "Cannot convert " ++ show v ++ " to bytecode."

serializeControl :: Control -> Put
serializeControl (val : vs) =
  foldl
  (\m v -> m >>= (\_ -> serializeValue v))
  (serializeValue val)
  vs

main :: IO ()
main = BL.putStr $ runPut $ serializeControl facTest

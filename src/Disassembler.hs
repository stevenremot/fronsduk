module Disassembler where

import Machine
import Bytecode
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL

disassembleValue :: Value -> String
disassembleValue (OperatorValue op) = show op
disassembleValue (NumberValue i) = show i
disassembleValue (ListValue l) = "(" ++ (disassembleControl l) ++ ")"

disassembleControl :: Control -> String
disassembleControl = tail . (foldl (++) "") . (map ((" "++).disassembleValue))

main :: IO ()
main = do
  input <- BL.getContents
  putStr $ disassembleControl $ runGet deserializeControl input

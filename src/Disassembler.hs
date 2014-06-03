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

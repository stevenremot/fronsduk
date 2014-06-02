module Interpreter where

import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Data.Maybe
import Machine
import Bytecode

main :: IO ()
main = do
  input <- BL.getContents
  print $ fromJust $ runControl $ runGet deserializeControl input

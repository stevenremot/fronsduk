module Interpreter where

import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Data.Maybe
import Machine
import Bytecode

main :: IO ()
main = do
  input <- BL.getContents
  result <- runControl $ runGet deserializeControl input
  print $ fromJust result

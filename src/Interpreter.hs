module Interpreter where

import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Data.Maybe
import System.Environment
import System.IO
import Machine
import Bytecode


interpret :: Handle -> IO ()
interpret handle = do
  input <- BL.hGetContents handle
  result <- runControl $ runGet deserializeControl input
  print $ fromJust result

main :: IO ()
main = do
  args <- getArgs
  if null args
    then interpret stdin
    else do
    handle <- openFile (head args) ReadMode
    interpret handle

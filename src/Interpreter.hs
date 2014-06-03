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

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

module Qzitchec where

import Machine
import Bytecode
import Qzitche.Parse
import Qzitche.Compile
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Put
import System.Environment
import System.IO

compile :: String -> Control
compile = compileSyntax.parseQzitche

compileInput :: Handle -> IO ()
compileInput handle = do
  input <- hGetContents handle
  BL.putStr $ runPut $ serializeControl $ compile input


main :: IO ()
main = do
  args <- getArgs
  if null args
    then compileInput stdin
    else do
    handle <- openFile (head args) ReadMode
    compileInput handle

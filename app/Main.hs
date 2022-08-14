module Main where
import System.Environment
import qualified Data.ByteString as BS 
import Coder
import Decoder
main = do
   [mode, inp, out] <- getArgs
   if (mode == "compress") then coder inp out
   else if (mode == "decompress") then decoder inp out
   else error "Wrong mode"
   putStrLn "Complete :)"
   return ()
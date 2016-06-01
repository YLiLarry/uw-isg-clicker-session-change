module Main where

import Analyze
import Parse
import Tokenize
import Control.Monad.Writer
import Control.Monad.Except

main :: IO ()
main = do 
   putStrLn "# The first day of term only needs to be earlier than the actual date,"
   putStrLn "# so 1858-11-17 is used and it should work as expected."
   putStrLn "# Input email below:"
   interact make

make :: String -> String   
make input = do
   case a of 
      Left e  -> error $ "\nAn Error happened:\n" ++
                         e ++
                         "This could happen because the format of emails has changed by the register office.\n" ++
                         "Check your input, or post a ticket on github.\n"
      Right s -> s
   where
      a = runExcept $ do
            (str, log) <- runWriterT $ do
               (parse $ tokenize input) >>= toClickerOverwrite
            return (str ++ log)
            
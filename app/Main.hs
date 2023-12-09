module Main (main) where

import Lib
import System.Environment

main :: IO ()
main = do
    [file1, file2, file3] <- getArgs
    catalog_src <- readFile file1
    cart_src <- readFile file2
    bonuscard_src <- readFile file3
    putStrLn $ printCheck (toItemList (splitBy '\n' catalog_src)) (toPositionList (splitBy '\n' cart_src)) (toBonusCard (splitBy ',' bonuscard_src))

    

module Main where

import System.Environment
import KCATS.Parser
import KCATS.Eval

main :: IO ()
main = do
    args <- getArgs
    case args of
        [file] -> do
            fileContents <- readFile file
            case parseKCATS file fileContents of
                Right r -> do 
                    print r
                    print $ runEval $ eval r
                Left s -> error s
        _ -> error "No file!"

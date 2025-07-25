module Main where

import System.Environment
import Data.List.Split
import Data.List
import Data.Int
import KCATS.AST
import KCATS.Parser
import KCATS.Eval
import KCATS.Inverter
import KCATS.Monad

memload :: [String] -> [INST]
memload [] = []
memload (memi:memlist:xs) = 
    (++ memload xs)
    $ foldr (\(i, v) l -> ADDI v : EXCHA i : l) []
    $ zip (iterate (+1) (read memi)) 
    $ map (read :: String -> Int64) 
    $ splitOn "," memlist 
memload _ = error "Each memory load should be two arguments!"

printMemory :: KCATS.Monad.State [] Int64 -> IO ()
printMemory s = do 
    if null $ primDataStack s 
        then putStrLn "Primary data stack was empty" 
        else do 
            putStr "Primary data stack contained the following: "
            print $ primDataStack s
    if null $ secDataStack s 
        then putStrLn "Secondary data stack was empty" 
        else do 
            putStr "Secondary data stack contained the following: "
            print $ secDataStack s
    if null $ retStack s 
        then putStrLn "Return stack was empty" 
        else do 
            putStr "Return stack contained the following: "
            print $ retStack s
    if null $ staticMemory s
        then putStrLn "Static memory is zero"
        else do 
            putStr "State of static memory (index, value): "
            print $ sortOn fst $ staticMemory s

instsToString :: [INST] -> String
instsToString = foldl ((++) . (++ "\n")) "" . map showINST 

main :: IO ()
main = do
    args <- getArgs
    case args of
        "--inv":infile:"-o":outfile:[] -> do 
            inFileContents <- readFile infile
            case parseKCATS infile inFileContents of
                Right r ->  writeFile outfile
                            $ (++) "; Generated by KCATS-inverter" 
                            $ instsToString 
                            $ invertInsts 
                            $ r
                Left s -> error s 
        file:loads -> do 
            loadList <- case loads of 
                "--mem":memoryloads -> pure $ memload memoryloads
                [] -> pure []
                s -> do 
                    putStr "Arguments: "
                    print s
                    putStrLn "Are not valid, you must use --mem ..."
                    error "Failed! Arguments wrong"
            fileContents <- readFile file
            case parseKCATS file fileContents of
                Right r -> do 
                    case splitWhen (== START) r of 
                        [xs, ys] -> printMemory $ runEval $ eval (xs ++ START : loadList ++ ys)
                        _ -> print "Error: Missing or too many START labels"
                Left s -> error s
        _ -> do 
            putStrLn "Usage:"
            putStr   "\tRunning interpreter: "
            putStrLn "\t<input_file> [--mem memindex memlist ...] "
            putStrLn "\tExample: \t\t\"./kcats prog.kc --mem 0 1,2,3,4 100 5,6,7,8\""
            putStrLn ""
            putStr   "\tInversion on file: "
            putStrLn "\t--inv <input_file> -o <output_file>"
            putStrLn "\tExample: \t\t\"./kcats --inv encode.kc -o decode.kc\""

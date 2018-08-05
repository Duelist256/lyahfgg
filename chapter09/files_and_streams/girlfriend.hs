import System.IO

-- main = do
--     withFile "girlfriend.txt" ReadMode (\handle -> do
--         contents <- hGetContents handle
--         putStr contents)

main = do  
    contents <- readFile "girlfriend.txt"  
    putStr contents

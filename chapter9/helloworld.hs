-- Hello world!
import Data.Char
import Control.Monad

-- main = do
--     putStrLn "Hello, what's your name?"
--     name <- getLine
--     putStrLn ("Hey " ++ name ++ ", you rock!")

-- main = do
--     putStrLn "What's your first name?"
--     firstName <- getLine
--     putStrLn "What's your last name?"
--     lastName <- getLine
--     let bigFirstName = map toUpper firstName
--         bigLastName = map toUpper lastName
--     putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"

-- main = do
--     line <- getLine
--     if null line
--         then return ()
--         else do
--             putStrLn $ reverseWords line
--             main
-- 
-- reverseWords :: String -> String
-- reverseWords = unwords . map reverse . words

-- main = do
--     a <- return "hell"
--     b <- return "yeah!"
--     putStrLn $ a ++ " " ++ b

-- main = do
--     c <- getChar
--     if c /= ' '
--         then do
--             putChar c
--             main
--         else return ()

-- main = do
--     c <- getChar
--     when (c /= ' ') $ do
--         putChar c
--         main

-- main = do
--     rs <- sequence [getLine, getLine, getLine]
--     print rs

-- main = forever $ do
--     putStr "Give me some input: "
--     l <- getLine
--     putStrLn $ map toUpper l

main = do
    colors <- forM [1,2,3,4] (\a -> do
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
        color <- getLine
        return color)
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
    mapM putStrLn colors

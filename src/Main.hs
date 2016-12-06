module Main where

import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Enter the cipher key: "
  key <- getLine
  putStr "Enter the word to cipher: "
  word <- getLine
  putStrLn $ vigenereCipher key word


vigenereCipher :: String -> String -> String
vigenereCipher code = zipWith addChar infiniteCode
  where
    infiniteCode = code ++ infiniteCode
    --Zip the infiniteCode and the word together
    toInt' c = fromEnum c - fromEnum 'a'
    toChar' i = toEnum (i + fromEnum 'a')
    addChar a b = toChar' $ (toInt' a + toInt' b) `mod` 26

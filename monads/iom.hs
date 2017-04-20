module Iom where

import Data.Char

line ln =
  (getLine >>= putStrLn) >> putStrLn ln

isNonNegInt :: String -> Bool
isNonNegInt x
  | length x == 0 = False
  | otherwise =
      let c:cs = x in
        isDigit c && (cs == "" || isNonNegInt cs)

ask :: IO ()
ask =
  putStrLn "Please enter a noneg int"

return_or_resurse :: String -> IO Int
return_or_resurse s =
  if isNonNegInt s then
    return (read s)
  else
    getInt

getInt =
  ask >> getLine >>= return_or_resurse


f :: String -> IO String
f s =
  return s

printHelloWorld :: IO ()
printHelloWorld = f "Hello World" >>= putStrLn


main = putStrLn "Gimme 2 lines" >> (getLine >>= line)

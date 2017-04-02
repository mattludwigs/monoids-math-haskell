{-# LANGUAGE LambdaCase #-}
module Main where

{-
  BCruypt: https://hackage.haskell.org/package/bcrypt
  ByteString: https://hackage.haskell.org/package/bytestring
-}
import Crypto.BCrypt (hashPasswordUsingPolicy, slowerBcryptHashingPolicy)
import Data.ByteString.Char8 (pack, unpack)
import Data.ByteString (ByteString)

{-
  Wanted to try out some lambda casing.
-}
printPassInfo :: Maybe ByteString -> IO ()
printPassInfo = \case
  Just newHash -> putStrLn $ unpack newHash
  Nothing -> putStrLn "fail"


main :: IO ()
main = do
  putStrLn "enter new password> "
  userPass <- getLine
  pass <- hashPasswordUsingPolicy slowerBcryptHashingPolicy $ pack userPass
  printPassInfo pass

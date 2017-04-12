module Main where

import Control.Applicative
import Data.Text (Text)
import Data.Map
import Data.Aeson

data Person =
  Person { firstName :: !Text
         , lastName :: !Text
         , age :: Int
         , likesPizza :: Bool
         } deriving Show


instance FromJSON Person where
  parseJSON (Object v) =
    Person <$> v .: "fristName"
           <*> v .: "lastName"
           <*> v .: "age"
           <*> v .: "likePizza"
  parseJSON _ = mzero


instance ToJson



main :: IO ()
main =
  putStrLn "Bob was here"

{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Main where

-- import Control.Applicative
import Data.Text (Text)
-- import Data.Map
import Data.Aeson
import GHC.Generics
-- {
--     "expand": "description,lead,issueTypes,url,projectKeys",
--     "self": "https://knledg.atlassian.net/rest/api/2/project/10100",
--     "id": "10100",
--     "key": "DEMO",
--     "name": "Demo",
--     "avatarUrls": {
--       "48x48": "https://knledg.atlassian.net/secure/projectavatar?avatarId=10324",
--       "24x24": "https://knledg.atlassian.net/secure/projectavatar?size=small&avatarId=10324",
--       "16x16": "https://knledg.atlassian.net/secure/projectavatar?size=xsmall&avatarId=10324",
--       "32x32": "https://knledg.atlassian.net/secure/projectavatar?size=medium&avatarId=10324"
--     },
--     "projectTypeKey": "software"
--   },


data Project = Project
  { expand :: !Text
  , self :: !Text
  , id :: !Text
  } deriving (Show, Generic)


instance FromJSON Project
instance ToJSON Project

main :: IO ()
main =
  putStrLn "Bob"

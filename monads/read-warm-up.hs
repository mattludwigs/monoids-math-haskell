module ReadWarmUp where


import Control.Monad.Reader
import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped =
  fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled = do
  c <- cap
  r <- rev
  return (c, r)



type Permissions = [String]

permissions = ["admin"]

data User = User
  { name :: String
  , age :: Integer
  , address :: String
  } deriving (Show)

createUser :: Reader Permissions (Maybe User)
createUser = do
  permissions <- ask
  if hasPermission "admin" permissions then
    return (Just $ User "Matt" 21 "Phx")
  else
    return Nothing

createUser' :: Permissions -> Maybe User
createUser' ps =
  if hasPermission "admin" ps then
    Just $ User "Matt" 21 "Phx"
  else
    Nothing

hasPermission :: String -> Permissions -> Bool
hasPermission per ps = elem per ps

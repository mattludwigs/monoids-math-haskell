module Indent where

import Control.Monad.Reader

type Level = Int
type Doc = Reader Level String

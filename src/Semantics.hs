{-# LANGUAGE ScopedTypeVariables #-}

module Semantics where

import Parser
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Exception (catch, SomeException)
import Data.Map (Map)
import Data.Complex
import qualified Data.Map as Map
import System.Random
import Text.Read (readMaybe)

-- Runtime values
data Value
    = VInt Int
    | VString String
    | VQubit [Complex Double]  -- STM transactional variable for resource locking
    deriving (Eq)

instance Show Value where
    show (VInt n) = show n
    show (VString s) = s
    show (VQubit vector) = show vector
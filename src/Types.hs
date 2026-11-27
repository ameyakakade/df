{-# LANGUAGE DataKinds #-}
module Types where

import           Control.Monad.State.Lazy
import           Control.Monad.Trans.Either
import           Data.List.NonEmpty
import qualified Data.Map                   as M
import qualified Data.Set                   as E

data Atom = Num Int
          | Str String
          | Cons Atom Atom 
          | Quote Atom
          | Nil
          deriving Eq

instance Show Atom where
    show a = case a of
               Num i -> show i
               Str i -> i
               Nil -> "nil"
               Quote i -> "'" ++ show i
               Cons _ _ -> "(" ++ showCons a ++ ")"
                 where showCons (Cons a Nil) = show a
                       showCons (Cons a b@(Cons _ _)) = show a ++ " " ++ showCons b
                       showCons (Cons a b) = show a ++ " . " ++ show b

type Result = StateP Atom

-- Function      args     error or result
data Fun = Unary (Atom -> Result)
         | Binary (Atom -> Atom -> Result)
         | Ternary (Atom -> Atom -> Result)

-- Stores map of string to scope no and atom
-- Stores current scope index
data ProgramState = ProgramState {
    definitions :: M.Map String [(Int, Atom)],
    scope       :: Int,
    trace       :: [String]
    } deriving (Show)

initState = ProgramState M.empty 0 []

type StateP a = EitherT String (StateT ProgramState IO) a

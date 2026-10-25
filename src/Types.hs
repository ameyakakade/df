module Types where

import           Control.Monad.State.Lazy
import           Control.Monad.Trans.Either
import           Data.List.NonEmpty
import qualified Data.Map                   as M
import qualified Data.Set                   as E

data Atom = Num Int
          | Str String
          | List [Atom] -- [] is nil or '()
          | Quote Atom
          deriving (Show, Eq)

type Result = Either String Atom

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

type StateP a = EitherT String (State ProgramState) a

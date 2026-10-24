module Types where

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

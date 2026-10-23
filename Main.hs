import Parser

data Atom = Num Int
          | Str String
          deriving (Show)

data List = Nil
          | Cons Atom List
          deriving (Show)

a = (Cons (Num 8) (Cons (Str "hello") Nil))

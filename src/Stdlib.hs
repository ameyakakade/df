{-# LANGUAGE LambdaCase #-}
module Stdlib where

import           Types

import qualified Data.Map as M

predefined :: M.Map String Fun
predefined = M.fromList [
    ("+", operators "+"),
    ("-", operators "-"),
    ("*", operators "*"),
    ("/", operators "/"),
    ("=", equals),
    ("car", car),
    ("cons", cons),
    ("cdr", cdr),
    ("negate", Stdlib.negate)
    ]

operators :: String -> Fun
operators s = Binary $ case s of
  "+" -> fn (+)
  "-" -> fn (-)
  "*" -> fn (*)
  "/" -> fn div
  where fn f (Num a) (Num b) = Right (Num (f a b))
        fn f _ _             = Left "Can apply only with numbers."

equals :: Fun
equals = Binary $ \a b -> Right $ if a == b then Str "t" else Nil

car :: Fun
car = Unary $ \case Cons a _ -> Right a; Nil -> Right Nil; _ -> Left "car function expects a cons cell or nil as argument"

cdr :: Fun
cdr = Unary $ \case Cons _ a  -> Right a; Nil -> Right Nil; _ -> Left "cdr function expects a cons cell or nil as argument"

cons :: Fun
cons = Binary $ \a b -> Right (Cons a b)

negate :: Fun
negate = Unary $ \case Num a -> Right (Num (-a)); _ -> Left "Negate function expects a number as argument"

consLen :: Atom -> Int
consLen (Cons _ Nil) = 1
consLen (Cons _ b) = 1 + consLen b
consLen _ = undefined

consZip :: Atom -> Atom -> Atom
consZip (Cons a as) (Cons b bs) = Cons (Cons a b) (consZip as bs)
consZip Nil _ = Nil
consZip _ Nil = Nil

listToCons :: [Atom] -> Atom
listToCons [] = Nil
listToCons (a:b) = Cons a (listToCons b)

consToList :: Atom -> [Atom]
consToList Nil = []
consToList (Cons a b) = a:consToList b

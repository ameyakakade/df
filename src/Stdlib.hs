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
  "/" -> fn (div)
  where fn f (Num a) (Num b) = Right (Num (f a b))
        fn f _ _             = Left "Can apply only with numbers."

equals :: Fun
equals = Binary $ \a b -> Right $ if a == b then Str "t" else List []

car :: Fun
car = Unary $ \case List a -> Right (if null a then List [] else head a); _ -> Left "car function expects a list as argument"

cdr :: Fun
cdr = Unary $ \case List a -> Right (if null a then List [] else List $ tail a); _ -> Left "cdr function expects a list as argument"

cons :: Fun
cons = Binary $ \a b -> case b of
                          List b -> Right $ List (a:b)
                          b      -> Right $ List (a:[b])

negate :: Fun
negate = Unary $ \case Num a -> Right (Num (-a)); _ -> Left "Negate function expects a number as argument"

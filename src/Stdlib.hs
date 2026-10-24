module Stdlib where

import Types

import qualified Data.Map as M

predefined :: M.Map String Fun
predefined = M.fromList [
    ("+", operators "+"),
    ("-", operators "-"),
    ("*", operators "*")
    ]

operators :: String -> Fun
operators s = Binary $ case s of
  "+" -> fn (+)
  "-" -> fn (-)
  "*" -> fn (*)
  where fn f (Num a) (Num b) = Right (Num (f a b))
        fn f _ _ = Left "Can apply only with numbers."

{-# LANGUAGE TemplateHaskell, NoImplicitPrelude #-}

module Core where

import Magic

------------------------
data Bool = False | True
  deriving Show

not :: Bool -> Bool
not True  = False
not False = True
{-
  case x of
    False -> True
    True  -> False
-}

and :: Bool -> Bool -> Bool
and False _ = False
and True y  = y

(&&) :: Bool -> Bool -> Bool
(&&) False _ = False
(&&) True y  = y

(||) :: Bool -> Bool -> Bool
(||) True _ = True
(||) False y  = y

--------------------------
data Nat = Zero | Succ Nat
  --deriving Show

magicNat ''Nat

(+) :: Nat -> Nat -> Nat
(+) Zero     y = y
(+) (Succ a) y = Succ (a + y)

even :: Nat -> Bool
even Zero     = True
even (Succ a) = odd a

odd :: Nat -> Bool
odd Zero     = False
odd (Succ a) = even a

-----------------------------
data Stream = Neck Nat Stream
  deriving Show

zeroes :: Stream
zeroes = Neck Zero zeroes

head :: Stream -> Nat
head (Neck x _) = x

tail :: Stream -> Stream
tail (Neck _ xs) = xs

nats :: Stream
nats = natsFrom Zero

natsFrom :: Nat -> Stream
natsFrom = \n ->
  Neck n (natsFrom (Succ n))

map :: (Nat -> Nat) -> (Stream -> Stream)
map f (Neck x xs) = Neck (f x) (map f xs)

ones :: Stream
ones = map Succ zeroes

filter :: (Nat -> Bool) -> (Stream -> Stream)
filter p s =
  case s of
    (Neck x xs) ->
      case p x of
        False -> filter p xs
        True  -> Neck x (filter p xs)

-- Home Task --

-- yes its here

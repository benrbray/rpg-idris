module Rpg.Model

--------------------------------------------------------------------------------

Map : Type -> Type -> Type
Map k v = List (k,v)

--------------------------------------------------------------------------------

data Die = D4 | D6 | D8 | D10 | D12 | D20

data Roll = MkRoll (List (Int, Die))

d : Int -> Die -> Roll
d x y = MkRoll [(x, y)]

Attribute = Int

record Sheet where
  constructor MkSheet
  foo : Int

--------------------------------------------------------------------------------

-- a period of time (usually, indicates the refresh interval for a resource)
data Period = Turns Int | LongRest | ShortRest

--------------------------------------------------------------------------------

RuleSet = (attrs : Type) -> Type
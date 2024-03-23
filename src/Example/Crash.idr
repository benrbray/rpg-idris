-- "A Crash Course in Idris 2"
-- https://idris2.readthedocs.io/en/latest/tutorial/index.html
module Example.Crash

---- types and functions -------------------------------------------------------

partial
fromMaybe : Maybe a -> a
fromMaybe (Just x) = ?result

-- where blocks can include local data declarations
-- foo : Int -> Int
-- foo x = case isLT of
--             Yes => x*2
--             No => x*4
--     where
--        data MyLT = Yes | No

--        isLT : MyLT
--        isLT = if x < 20 then Yes else No

-- dependent types

isSingleton : Bool -> Type
isSingleton True = Nat
isSingleton False = List Nat

mkSingle : (x : Bool) -> isSingleton x
mkSingle True = 0
mkSingle False = []

sum : (single : Bool) -> isSingleton single -> Nat
sum True x = x
sum False [] = 0
sum False (x :: xs) = x + sum False xs

-- vectors

data Vect : Nat -> Type -> Type where
  Nil  : Vect Z a
  (::) : a -> Vect k a -> Vect (S k) a

(++) : Vect n a -> Vect m a -> Vect (n + m) a
(++) Nil       ys = ys
(++) (x :: xs) ys = x :: (xs ++ ys)

-- finite sets

data Fin : Nat -> Type where
  FZ : Fin (S k)
  FS : Fin k -> Fin (S k)

index : forall a,n. Fin n -> Vect n a -> a
index FZ     (x :: xs) = x
index (FS k) (x :: xs) = index k xs

-- mutual recursion

mutual
  even : Nat -> Bool
  even Z = True
  even (S k) = odd k

  odd : Nat -> Bool
  odd Z = False
  odd (S k) = even k

-- forward declarations

even2 : Nat -> Bool
odd2 : Nat -> Bool

even2 Z = True
even2 (S k) = odd2 k

odd2 Z = False
odd2 (S k) = even2 k

-- laziness

eagerIte : Bool -> a -> a -> a
eagerIte True  t f = t
eagerIte False t f = f

lazyIte : Bool -> Lazy a -> Lazy a -> a
lazyIte True  t f = t
lazyIte False t f = f

---- dependent pairs / sigma types ---------------------------------------------

-- a dependent pair of a vector with its length
pairA : (n : Nat ** Vect n Int)
pairA = (2 ** [3,4])

-- the typechecker can also infer the length
pairB : (n : Nat ** Vect n Int)
pairB = (_ ** [3,4])

pairC : Bool -> (n : Nat ** Vect n Int)
pairC True = (_ ** [3,4])
pairC False = (_ ** [1,2,3])

filter : (a -> Bool) -> Vect n a -> (p ** Vect p a)
filter p Nil = (_ ** [])
filter p (x :: xs) =
  case filter p xs of
    (_ ** ys) =>
      if p x then (_ ** x :: ys)
             else (_ ** ys)

---- records -------------------------------------------------------------------

record Person where
  constructor MkPerson
  firstName, middleName, lastName : String
  age : Int

benjamin : Person
benjamin = MkPerson "Benjamin" "Russell" "Bray" 28
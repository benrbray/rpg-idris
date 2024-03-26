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
  firstName, lastName : String
  age : Int

johnSmith : Person
johnSmith = MkPerson "Benjamin" "Bray" 28

johnMulaney : Person
johnMulaney = { lastName := "Mulaney", age $= (+11) } johnSmith

-- record parameters
record Prod a b where
  constructor Times
  fst : a
  snd : b

-- dependent record

record DPair a (p : a -> Type) where
  constructor MkDPair
  fst : a
  snd : p fst

cons : t -> (x : Nat ** Vect x t) -> (x : Nat ** Vect x t)
cons val xs =
  { fst := S (fst xs)
  , snd := (val :: snd xs) } xs

---- idiom brackets ----------------------------------------

m_app : Maybe (a -> b) -> Maybe a -> Maybe b
m_app (Just f) (Just a) = Just (f a)
m_app _        _        = Nothing

m_add : Maybe Int -> Maybe Int -> Maybe Int
m_add x y = m_app (m_app (Just (+)) x) y

-- equivalent to m_add2 but uses the Applicative Maybe instance
m_add2 : Maybe Int -> Maybe Int -> Maybe Int
m_add2 x y = [| x + y |]
-- m_add2 x y = pure (+) <*> x <*> y

data Expr
  = Var String
  | Val Int
  | Add Expr Expr

-- wrapping the evaluator in a datatype means we can provide
-- implementations of interfaces (Functor, Applicative) for it later
data Eval : Type -> Type where
  MkEval : (List (String, Int) -> Maybe a) -> Eval a

fetch : String -> Eval Int
fetch x = MkEval fetchVal
  where
    fetchVal : List (String, Int) -> Maybe Int
    fetchVal [] = Nothing
    fetchVal ((v,val) :: xs) =
      if (x == v)
        then (Just val)
        else (fetchVal xs)

Functor Eval where
  map f (MkEval g) = MkEval (\e => map f (g e))

Applicative Eval where
  pure x = MkEval (\e => Just x)

  (<*>) (MkEval f) (MkEval g) = MkEval (\x => app (f x) (g x))
    where
      app : Maybe (a -> b) -> Maybe a -> Maybe b
      app (Just fx) (Just gx) = Just (fx gx)
      app _         _         = Nothing

eval : Expr -> Eval Int
eval (Var x) = fetch x
eval (Val x) = [| x |]
eval (Add x y) = [| eval x + eval y |]

runEval : List (String, Int) -> Expr -> Maybe Int
runEval env e = case eval e of
  MkEval envFn => envFn env
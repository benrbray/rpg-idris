module Example.BuildSystems

import Control.Monad.State
import Data.SortedMap

--------------------------------------------------------------------------------

-- describes a single build task
data Task
  :  (c : (Type -> Type) -> Type) -- constraint
  -> (k : Type)                   -- key
  -> (v : Type)                   -- value
  -> Type
  where
    MkTask :
      (forall f. c f =>       -- using effects limited to those described by `c`,
        (fetch : k -> f v) -> -- and given a way of building its dependencies,
        f v                   -- a task knows how to build itself
      ) -> Task c k v

-- associates a task with every non-input key
-- (Nothing corresponds to an input that is assumed to exist)
Tasks c k v = k -> Maybe (Task c k v)

run : c f => Task c k v -> (k -> f v) -> f v
run (MkTask task) fetch = task fetch

---- store ---------------------------------------------------------------------

-- abstract store containing a key/value map and persistent build information
record Store i k v where
  constructor MkStore
  info : i
  inputValues : k -> v
  store : SortedMap k v

initStore : Ord k => i -> (k -> v) -> Store i k v
initStore info inp = MkStore info inp empty

getInfo  : Store i k v -> i
getInfo s = s.info

putInfo  : i -> Store i k v -> Store i k v
putInfo i = { info := i }

getStore : k -> Store i k v -> v
getStore k s = case lookup k s.store of
  Nothing => s.inputValues k
  Just v => v

putStore : Eq k => k -> v -> Store i k v -> Store i k v
putStore k v s = { store := insert k v s.store } s

-- build system
Build c i k v =
  Tasks c k v -> -- task descriptions
  k           -> -- target key
  Store i k v -> -- initial store
  Store i k v    -- new store where value of target key has been updated

---- busy build system ---------------------------------------------------------

-- terminates with a correct result, but is not *minimal*,
-- as it is memoryless (i=()), recomputing values as needed
covering
buildBusy : Eq k => Build Applicative () k v
buildBusy tasks key init = execState init (fetch key)
  where
    fetch : k -> State (Store () k v) v
    fetch k = case tasks k of
      Nothing => do
      -- corresponds to an input that is assumed to exist
        gets (getStore k)
      Just task => do
        -- prerequite task, run immediately
        v <- run task fetch
        modify $ putStore k v
        pure v


---- spreadsheet example -------------------------------------------------------

-- A1 = 10    B1 = A1 + A2
-- A2 = 20    B2 = B1 * 2
sprsh1 : Tasks Applicative String Integer
sprsh1 "B1" = Just . MkTask $ \fetch => [| fetch "A1" + fetch "A2" |]
sprsh1 "B2" = Just . MkTask $ \fetch => [| fetch "B1" * pure 2     |]
sprsh1 _    = Nothing

initialSprsh : Store () String Integer
initialSprsh = initStore () (\key => if key == "A1" then 10 else 20)

-- new store where B2 has most up to date value
result = buildBusy sprsh1 "B2" initialSprsh

--------------------------------------------------------------------------------

export
runExample : List String
runExample = show <$> [
    getStore "B1" result,
    getStore "B2" result
  ]
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Algorithm.Search2 where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Foldable as Foldable
import Data.Maybe

newtype LIFOHeap k a = LIFOHeap { getLIFOHeap :: Map.Map k [a] }

emptyLIFOHeap :: LIFOHeap k a
emptyLIFOHeap = LIFOHeap Map.empty

-- | This is just a convenience function which @fmap@s two deep
fmap2 :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
fmap2 = fmap . fmap

-- | @leastCostly paths_a paths_b@ is a utility function to be used with
-- 'dijkstra'-like functions. It returns True when the cost of @paths_a@
-- is less than the cost of @paths_b@, where the total costs are the first
-- elements in each tuple in each path
leastCostly :: Ord a => [(a, b)] -> [(a, b)] -> Bool
leastCostly ((cost_a, _):_) ((cost_b, _):_) = cost_b < cost_a
-- logically this never happens, because if you have a
-- zero-length path a point, you already visited it
-- and thus do not consider other paths to it
leastCostly [] _ = False
-- logically this never happens, because you cannot find
-- a new zero-length path to a point
leastCostly _ [] = True

-- | The @SearchContainer@ class abstracts the idea of a container to be used in
-- @generalizedSearch@
class SearchContainer container where
  type Elem container
  pop :: container -> Maybe (Elem container, container)
  push :: container -> Elem container -> container


instance (Show cost, Show state) => Show (LIFOHeap cost (cost, state)) where
  show (LIFOHeap inner) = show inner

instance SearchContainer [a] where
  type Elem [a] = a
  pop list =
    case list of
      [] -> Nothing
      (x : xs) -> Just (x, xs)
  push list a = a : list

instance Ord k => SearchContainer (LIFOHeap k a) where
  type Elem (LIFOHeap k a) = (k, a)
  pop (LIFOHeap inner)
    | Map.null inner = Nothing
    | otherwise = case Map.findMin inner of
      (k, [a]) -> Just ((k, a), LIFOHeap $ Map.deleteMin inner)
      (k, a : _) -> Just ((k, a), LIFOHeap $ Map.updateMin (Just . tail) inner)
      (_, []) -> pop (LIFOHeap $ Map.deleteMin inner)
                 -- Logically, this should never happen
  push (LIFOHeap inner) (k, a) = LIFOHeap $ Map.insertWith (++) k [a] inner

-- | A @SearchState@ represents the state of a generalized search at a given
-- point in an algorithms execution. The advantage of this abstraction is that
-- it can be used for things like bidirectional searches, where you want to
-- stop and start a search part-way through.
data SearchState container stateKey state = SearchState {
  current :: state,
  queue :: container,
  visited :: Set.Set stateKey,
  paths :: Map.Map stateKey [state]
}

findIterate :: (a -> IO (Maybe a)) -> (a -> Bool) -> a -> IO (Maybe a)
findIterate next found initial = if found initial then (return $ Just initial) else (next initial >>= maybe (return Nothing) (findIterate next found))

nextSearchState ::
  (Foldable f, SearchContainer container, Show container, Ord stateKey, Show stateKey, Elem container ~ state, Show state)
  => ([state] -> [state] -> Bool)
  -> (state -> stateKey)
  -> (state -> f state)
  -> SearchState container stateKey state
  -> IO (Maybe (SearchState container stateKey state))
nextSearchState better mk_key next old = do
  print $ fmap queue new_state_May
  case new_state_May of
    Just new_state ->
      if mk_key (current new_state) `Set.member` visited old
      then nextSearchState better mk_key next new_state
      else return $ Just new_state
    Nothing -> return $ Nothing
  where
    mk_search_state new_paths (new_current, remaining_queue) = SearchState {
      current = new_current,
      queue = remaining_queue,
      visited = Set.insert (mk_key new_current) (visited old),
      paths = new_paths
    }
    (new_queue, new_paths) = List.foldl' update_queue_paths (queue old, paths old) (next (current old))
    new_state_May = mk_search_state new_paths <$> pop new_queue
    update_queue_paths (old_queue, old_paths) st =
      if mk_key st `Set.member` visited old
      then (old_queue, old_paths)
      else
        case Map.lookup (mk_key st) old_paths of
          Just old_path ->
            if better old_path (st : steps_so_far)
            then (q', ps')
            else (old_queue, old_paths)
          Nothing -> (q', ps')
        where
          steps_so_far = paths old Map.! mk_key (current old)
          q' = push old_queue st
          ps' = Map.insert (mk_key st) (st : steps_so_far) old_paths

-- | @generalizedSearchM@ is a monadic version of generalizedSearch
generalizedSearch ::
  (Foldable f, SearchContainer container, Show container, Ord stateKey, Show stateKey,
   Elem container ~ state, Show state)
  => container
  -- ^ Empty @SearchContainer@
  -> (state -> stateKey)
  -- ^ Function to turn a @state@ into a key by which states will be compared
  -- when determining whether a state has be enqueued and / or visited
  -> ([state] -> [state] -> Bool)
  -- ^ Function @better old new@, which when given a choice between an @old@ and
  -- a @new@ path to a state, returns True when @new@ is a "better" path than
  -- old and should thus be inserted
  -> (state -> f state)
  -- ^ Function to generate "next" states given a current state
  -> (state -> Bool)
  -- ^ Predicate to determine if solution found. @generalizedSearch@ returns a
  -- path to the first state for which this predicate returns 'True'.
  -> state
  -- ^ Initial state
  -> IO (Maybe [state])
  -- ^ First path found to a state matching the predicate, or 'Nothing' if no
  -- such path exists.
generalizedSearch empty mk_key better next found initial = do
  let get_steps search_st = paths search_st Map.! mk_key (current search_st)
  let initial_state = SearchState initial empty (Set.singleton $ mk_key initial) (Map.singleton (mk_key initial) [])
  end_May <- findIterate (nextSearchState better mk_key next) (found . current) initial_state
  return $ fmap (reverse . get_steps) end_May
    

aStarAssoc :: (Num cost, Ord cost, Show cost, Ord state, Show state)
  => (state -> [(state, cost)])
  -- ^ function to generate list of neighboring states with associated
  -- transition costs given the current state
  -> (state -> cost)
  -- ^ Estimate on remaining cost given a state
  -> (state -> Bool)
  -- ^ Predicate to determine if solution found. 'aStar' returns the shortest
  -- path to the first state for which this predicate returns 'True'.
  -> state
  -- ^ Initial state
  -> IO (Maybe (cost, [state]))
  -- ^ (Total cost, list of steps) for the first path found which satisfies the
  -- given predicate
aStarAssoc next remaining found initial = do
  -- This API to A* search is useful in the common case when next
  -- states and their associated transition costs are generated together.
  --
  -- A* can be viewed as a generalized search, with the search container being a
  -- heap, with the states being compared without regard to cost, with the
  -- shorter paths taking precedence over longer ones, and with
  -- the stored state being (total cost estimate, (cost so far, state)).
  -- This implementation makes that transformation, then transforms that result
  -- back into the desired result from @aStarAssoc@
  r <- generalizedSearch emptyLIFOHeap snd2 leastCostly next'(found . snd2) (remaining initial, (0, initial))
  return $ unpack <$> r
  where
    next' (_, (old_cost, old_st)) =
      update_state <$> (next old_st)
      where
        update_state (new_st, cost) =
          let new_cost = old_cost + cost
              new_est = new_cost + remaining new_st
          in (new_est, (new_cost, new_st))
    unpack [] = (0, [])
    unpack packed_states =
      (fst . snd . last $ packed_states, map snd2 packed_states)
    snd2 = snd . snd

aStar :: (Foldable f, Num cost, Ord cost, Show cost, Ord state, Show state)
  => (state -> f state)
  -- ^ Function to generate list of neighboring states given the current state
  -> (state -> state -> cost)
  -- ^ Function to generate transition costs between neighboring states. This is
  -- only called for adjacent states, so it is safe to have this function be
  -- partial for non-neighboring states.
  -> (state -> cost)
  -- ^ Estimate on remaining cost given a state
  -> (state -> Bool)
  -- ^ Predicate to determine if solution found. 'aStar' returns the shortest
  -- path to the first state for which this predicate returns 'True'.
  -> state
  -- ^ Initial state
  -> IO (Maybe (cost, [state]))
  -- ^ (Total cost, list of steps) for the first path found which satisfies the
  -- given predicate
aStar next cost remaining found initial = do
  aStarAssoc next' remaining found initial
  where
    next' st = map (\new_st -> (new_st, cost st new_st)) $ Foldable.toList (next st)

module Graphs where

-- TODO Data.Map.Strict from containers ?
import qualified RIO.Map as M
import qualified RIO.Set as S

-- TODO
-- type DistancesAndPrevs a = M.Map (Vertex a) (Int, Vertex a)

type Distances a = M.Map (Vertex a) Int

type Prevs a = M.Map (Vertex a) (Vertex a)

newtype Vertex a = Vert a
  deriving (Ord, Eq, Show)

type Neighbors a = S.Set (Int, Vertex a)

class Graph g where
  vertices :: Ord a => g a -> S.Set (Vertex a)

  neighbors :: Ord a => g a -> Vertex a -> Neighbors a

-- TODO as newtype
data MapGraph a = MapGraph (M.Map (Vertex a) (Neighbors a))

instance Graph MapGraph where
  vertices (MapGraph m) = S.union (M.keysSet m) $ S.unions $ map (S.map snd) $ map snd $ M.toList m

  neighbors (MapGraph m) v = M.findWithDefault S.empty v m

dijkstra :: (Graph g, Ord a) => g a -> Vertex a -> (Distances a, Prevs a)
dijkstra graph start =
  let q = vertices graph
      dist = M.singleton start 0
   in aux q dist M.empty
  where
    aux q dist prev =
      if S.null q
        then (dist, prev)
        else
          let vminMaybe = foldr (minIn dist) Nothing q
           in case vminMaybe of
                Nothing -> (dist, prev)
                Just vmin ->
                  let q' = S.delete vmin q
                      ns = neighbors graph vmin
                      (dist', prev') = foldr (insertBetter vmin) (dist, prev) ns
                   in aux q' dist' prev'

minIn _ v Nothing = Just v
minIn dist v (Just vmin) =
  case (M.lookup v dist, M.lookup vmin dist) of
    (Just d, Just dmin) -> Just $ if d < dmin then v else vmin
    (Nothing, _) -> Just vmin
    (_, Nothing) -> Just v
    _ -> Nothing

insertBetter v (distance, vn) (dist, prev) =
  case (M.lookup v dist, M.lookup vn dist) of
    (Just d, Just dn) ->
      let alt = d + distance
       in if alt < dn
            then (M.insert vn alt dist, M.insert vn v prev)
            else (dist, prev)
    (Just d, Nothing) ->
        let alt = d + distance
        in (M.insert vn alt dist, M.insert vn v prev)
    _ -> (dist, prev)

graph = MapGraph $ M.fromList [
    (Vert 'A', S.fromList [(4, Vert 'C'), (3, Vert 'B')]),
    (Vert 'B', S.fromList [(2, Vert 'C'), (3, Vert 'A')]),
    (Vert 'C', S.fromList [(4, Vert 'A'), (2, Vert 'B')])
    ]    
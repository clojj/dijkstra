module Graphs where

-- TODO Data.Map.Strict from containers ?
import qualified RIO.Map as M
import qualified RIO.Set as S
import Text.Pretty.Simple (pPrint)

type DistancesAndPrevs a = M.Map (Vertex a) (Int, Vertex a)

newtype Vertex a = Vert a
  deriving (Ord, Eq, Show)

type Neighbor a = (Int, Vertex a)

type Neighbors a = S.Set (Neighbor a)

class Graph g where
  vertices :: Ord a => g a -> S.Set (Vertex a)

  neighbors :: Ord a => g a -> Vertex a -> Neighbors a

-- TODO as newtype
data MapGraph a = MapGraph (M.Map (Vertex a) (Neighbors a))

instance Graph MapGraph where
  vertices (MapGraph m) = S.union (M.keysSet m) $ S.unions $ map (S.map snd) $ map snd $ M.toList m

  neighbors (MapGraph m) v = M.findWithDefault S.empty v m

dijkstra :: (Graph g, Ord a) => g a -> Vertex a -> DistancesAndPrevs a
dijkstra graph start =
  let q = vertices graph
      dap = M.singleton start (0, start)
   in aux q dap
  where
    aux q dap =
      if S.null q
        then dap
        else
          let vminMaybe = foldr (minIn dap) Nothing q
           in case vminMaybe of
                Nothing -> dap
                Just vmin ->
                  let q' = S.delete vmin q
                      ns = neighbors graph vmin
                      dap' = foldr (insertBetter vmin) dap ns
                   in aux q' dap'

minIn :: Ord a => DistancesAndPrevs a -> Vertex a -> Maybe (Vertex a) -> Maybe (Vertex a)
minIn _ v Nothing = Just v
minIn dap v (Just vmin) =
  case (M.lookup v dap, M.lookup vmin dap) of
    (Just (d, _), Just (dmin, _)) -> Just $ if d < dmin then v else vmin
    (Nothing, _) -> Just vmin
    (_, Nothing) -> Just v
    _ -> Nothing

insertBetter :: Ord a => Vertex a -> Neighbor a -> DistancesAndPrevs a -> DistancesAndPrevs a
insertBetter v (distance, vn) dap =
  case (M.lookup v dap, M.lookup vn dap) of
    (Just (d, _), Just (dn, _)) ->
      let alt = d + distance
       in if alt < dn
            then M.insert vn (alt, v) dap
            else dap
    (Just (d, _), Nothing) ->
      let alt = d + distance
       in M.insert vn (alt, v) dap
    _ -> dap

vA = Vert 'A'

vB = Vert 'B'

vC = Vert 'C'

graphExample :: MapGraph Char
graphExample =
  MapGraph $
    M.fromList
      [ (vA, S.fromList [(10, vC), (3, vB)]),
        (vB, S.fromList [(2, vC), (3, vA)]),
        (vC, S.fromList [(10, vA), (2, vB)])
      ]

test :: IO ()
test = pPrint $ dijkstra graphExample (Vert 'A')

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main,
  )
where

import qualified Data.OrdPSQ as PQ
import Import
import Options.Applicative.Simple
import qualified Paths_dijkstra
import RIO.Process
import Run

import qualified RIO.List as L
import qualified RIO.Map as M
import qualified RIO.Set as S

-- TODO
-- import Graphs

-- some vertices

vA = Keyed 'A'

vB = Keyed 'B'

vC = Keyed 'C'

-- psqueues

pq :: PQ.OrdPSQ (Vertex Char) Int Text
pq =
  let q1 = PQ.empty
      q2 = PQ.insert vA 5 "element 5" q1
      q3 = PQ.insert vB 2 "element 2" q2
   in PQ.insert vC 1 "element 1" q3

-- algorithm

newtype Vertex a = Keyed a
  deriving (Ord, Eq, Show)

newtype MapGraph a = Weighted {toMap :: M.Map (Vertex a) (S.Set (Int, Vertex a))}

vertices :: Ord a => MapGraph a -> S.Set (Vertex a)
vertices (Weighted m) =
  S.union (M.keysSet m) (S.unions (L.map (S.map snd) (L.map snd (M.toList m))))

graph' :: MapGraph Char
graph' = Weighted $ M.fromList [(Keyed 'A', S.fromList [(10, Keyed 'B'), (3, Keyed 'C')])]

-- ERROR ? S.Set (Int, Vertex a) does allow to reach neighbors by more than 1 edge !?

-- newtype ConnVertex a = ConnVertex (Int, Vertex a)
--   deriving Show

-- instance Eq a => Eq (ConnVertex a) where
--   (==) (ConnVertex (_, k1)) (ConnVertex (_, k2)) = k1 == k2

-- instance Ord a => Ord (ConnVertex a) where
--   (compare) (ConnVertex (_, k1)) (ConnVertex (_, k2)) = compare k1 k2

-- testSet :: S.Set (ConnVertex Char)
-- testSet = S.fromList [ConnVertex (1, Keyed 'A'), ConnVertex (2, Keyed 'A')]

testSet :: S.Set (Int, Vertex Char)
testSet = S.fromList [(1, Keyed 'A'), (1, Keyed 'A')]

main :: IO ()
main = do
  (options, ()) <-
    simpleOptions
      $(simpleVersion Paths_dijkstra.version)
      "Header for command line arguments"
      "Program description, also for command line arguments"
      ( Options
          <$> switch
            ( long "verbose"
                <> short 'v'
                <> help "Verbose output?"
            )
      )
      empty
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf,
            appProcessContext = pc,
            appOptions = options
          }
     in runRIO app run

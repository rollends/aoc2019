{-# LANGUAGE NoImplicitPrelude #-}

module Day6
  ( day6, day6part2
  ) where

import Control.Monad.Zip (mzipWith)

import RIO
import qualified RIO.List as L
import qualified RIO.List.Partial as L'
import qualified RIO.Text as T

import RIO.List.Partial ((!!))

import Util

type Name = Text
data OrbitMap =
    HeavyBody Name [OrbitMap]
  | Satellite Name

day6 :: [(Name, Name)] -> Int
day6 orbitPairs =
  let
    orbitmap = makeOrbitMap orbitPairs
  in
    countTotalOrbits orbitmap

day6part2 :: [(Name, Name)] -> Int
day6part2 orbitPairs =
  let
    orbitmap = makeOrbitMap orbitPairs
  in
    case orbitalTransfersToSanta orbitmap of
      Just k -> k
      Nothing -> -1

-- Finds number of orbital transfers from "YOU" to "SAN"
orbitalTransfersToSanta :: OrbitMap -> Maybe Int
orbitalTransfersToSanta omap =
  _orbitalTransfersToSanta 0 omap

_orbitalTransfersToSanta k (Satellite name) =
  if name == T.pack "YOU" || name == T.pack "SAN" then
    Just (k-1)
  else
    Nothing

_orbitalTransfersToSanta k (HeavyBody name children) =
  if name == T.pack "YOU" || name == T.pack "SAN" then
    Just (k-1)
  else
    let
      -- Recursive step
      childTransfers = map (_orbitalTransfersToSanta (k+1)) children
      -- Filter out paths that didn't find "YOU" or "SAN".
      goodChildren = filter isJust childTransfers
    in
      case length goodChildren of
        0 -> Nothing
        1 -> L'.head goodChildren -- Only found one of "YOU" or "SAN" or got result. Just pass it up.
        2 ->
          -- Found both "YOU" and "SAN". We are the common parent.
          -- Add their distances to the root and then subtract our distance to the root
          -- to get path length.
          mzipWith (+) (Just $ -2*k) $ mzipWith (+) (goodChildren !! 0) (goodChildren !! 1)

-- Counts the total number of direct+indirect orbits.
countTotalOrbits :: OrbitMap -> Int
countTotalOrbits orbitmap =
  _countTotalOrbits 0 orbitmap

-- Keeps track of distance to COM in recursion so we know
-- how many (indirect/direct) orbits to add!
_countTotalOrbits distanceToCOM (Satellite _) =
  distanceToCOM

_countTotalOrbits distanceToCOM (HeavyBody _ children) =
  distanceToCOM
  +
  (sum $ map (_countTotalOrbits (distanceToCOM + 1)) children)


makeOrbitMap :: [(Name, Name)] -> OrbitMap
makeOrbitMap pairs = _makeOrbitMap pairs (Satellite $ T.pack "COM", [])

_makeOrbitMap [] (omap, _) = omap
_makeOrbitMap (orbitPair:rest) (omap, orphans) =
  _makeOrbitMap rest $
    insert omap orphans orbitPair

-- Inserts into orbit map (tree) another element.
-- First parameter is the current map.
-- Second parameter is a list of orphans to check first!
-- Third parameter is connection to insert
insert :: OrbitMap -> [OrbitMap] -> (Name, Name) -> (OrbitMap, [OrbitMap])
insert omap orphans (parentName, childName) =
  let
    newNode = Satellite childName
    newParentNode = HeavyBody parentName [newNode]
    cleanedOrphans = filter (not . (isBody childName)) orphans
  in
    -- Find if the child to insert is _already_ an orphaned object. If so,
    -- we _must_ use that child object and then remove it from the list.
    case L.find (isBody childName) orphans of
      Just childNode ->
        -- See if we can insert the object normally into the current map.
        case tryInsertNode omap parentName childNode of
          (newMap, True) ->
            (newMap, cleanedOrphans)
          (_, False) ->
            -- See if we can insert the child as a child of one of the orphans
            case tryInsertNodeIntoMaps cleanedOrphans parentName childNode of
              (newOrphans, True) ->
                (omap, newOrphans)
              (_, False) ->
                -- We need to add both the child and parent as orphans!
                (omap, cleanedOrphans ++ [HeavyBody parentName [childNode]])
      Nothing ->
        -- See if we can insert the object normally into the current map.
        case tryInsertNode omap parentName newNode of
          (newMap, True) ->
            (newMap, orphans)
          (_, False) ->
            -- See if we can insert the child as a child of one of the orphans
            case tryInsertNodeIntoMaps orphans parentName newNode of
              (newOrphans, True) ->
                (omap, newOrphans)
              (_, False) ->
                -- We need to add both the child and parent as orphans!
                (omap, orphans ++ [newParentNode])

tryInsertNodeIntoMaps :: [OrbitMap] -> Name -> OrbitMap -> ([OrbitMap], Bool)
tryInsertNodeIntoMaps maps parentName node =
  let
    foldop = L.foldl (\(x,y) (a,b) -> (x++[a], y || b)) ([], False)
    insertop = \m -> tryInsertNode m parentName node
  in
    foldop . (L.map insertop) $ maps

tryInsertNode :: OrbitMap -> Name -> OrbitMap -> (OrbitMap, Bool)
tryInsertNode omap parentName node =
  let
    recursiveInsert = \child -> tryInsertNode child parentName node
    insertInChildren = (L.foldl (\(x,y) (a,b) -> (x++[a], y || b)) ([], False)). (L.map recursiveInsert)
  in
    case omap of
      HeavyBody name children ->
        if name == parentName then
          (HeavyBody name (children ++ [node]), True)
        else
          let
            (newChildren, happened) = insertInChildren children
          in
            (HeavyBody name newChildren, happened)
      Satellite name ->
        if name == parentName then
          (HeavyBody name [node], True)
        else
          (Satellite name, False)

isBody :: Name -> OrbitMap -> Bool
isBody name omap =
  case omap of
    HeavyBody oname _ -> name == oname
    Satellite oname   -> name == oname

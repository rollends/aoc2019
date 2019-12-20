{-# LANGUAGE NoImplicitPrelude #-}

module Day6
  ( day6
  ) where

import RIO
import qualified RIO.List as L
import qualified RIO.List.Partial as L'
import qualified RIO.Text as T

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

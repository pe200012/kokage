{-# LANGUAGE OverloadedStrings #-}

-- | Pure collision detection functions.
-- No IO, no GTK dependencies - just geometry.
module Kokage.Collision
  ( hitTestShape
  , findCollisionAt
  , pointInPolygon
  ) where

import Data.Foldable ( find )
import Data.List ( sortBy )
import Data.Ord ( comparing )

import Types.Ghost ( CollisionRegion(..), CollisionShape(..) )


-- | Test if a point (x, y) is inside a collision shape.
hitTestShape :: Int -> Int -> CollisionShape -> Bool
hitTestShape x y shape = case shape of
  CollisionRect x1 y1 x2 y2 ->
    x >= min x1 x2 && x <= max x1 x2 &&
    y >= min y1 y2 && y <= max y1 y2

  CollisionEllipse x1 y1 x2 y2 ->
    -- Ellipse bounded by rectangle
    let cx = fromIntegral (x1 + x2) / 2.0 :: Double
        cy = fromIntegral (y1 + y2) / 2.0 :: Double
        rx = fromIntegral (abs (x2 - x1)) / 2.0 :: Double
        ry = fromIntegral (abs (y2 - y1)) / 2.0 :: Double
        dx = (fromIntegral x - cx) / rx
        dy = (fromIntegral y - cy) / ry
    in rx > 0 && ry > 0 && (dx * dx + dy * dy) <= 1.0

  CollisionCircle cx cy r ->
    let dx = x - cx
        dy = y - cy
    in dx * dx + dy * dy <= r * r

  CollisionPolygon vertices ->
    pointInPolygon x y vertices

  CollisionRegionFile {} ->
    -- Region file hit testing requires loading the image
    -- Not implemented for now
    False

-- | Point-in-polygon test using ray casting algorithm.
pointInPolygon :: Int -> Int -> [ ( Int, Int ) ] -> Bool
pointInPolygon _ _ [] = False
pointInPolygon px py vertices = odd $ length $ filter crossesRay edges
  where
    edges = zip vertices (drop 1 vertices ++ take 1 vertices)

    crossesRay (( x1, y1 ), ( x2, y2 )) =
      let -- Check if the horizontal ray from (px, py) to the right crosses this edge
          minY = min y1 y2
          maxY = max y1 y2
          -- Point must be in the Y range of the edge (exclusive on top)
          inYRange = py >= minY && py < maxY
          -- Calculate X intersection of ray with edge
          xIntersect = fromIntegral x1
                     + (fromIntegral (py - y1) * fromIntegral (x2 - x1))
                     / fromIntegral (y2 - y1) :: Double
      in inYRange && fromIntegral px < xIntersect

-- | Find the first collision region that contains the given point.
-- Collision regions are checked in order of their index (lower index = higher priority).
findCollisionAt :: Int -> Int -> [ CollisionRegion ] -> Maybe CollisionRegion
findCollisionAt x y regions =
  let sorted = sortBy (comparing crIndex) regions
  in find (\cr -> hitTestShape x y (crShape cr)) sorted

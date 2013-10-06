{-# LANGUAGE ViewPatterns #-}

module Utilities where

import Graphics.Gloss ( Point )

import Vector

int = read :: String -> Int

-- View Pattern Aliases for vectors
v :: Point -> Vec2F
v = toVec

p :: Vec2F -> Point
p = toPoint

s :: Float -> Vec2F
s = S

s' :: Vec2F -> Float
s' (S a) = a
s' _ = error "Not a scalar"

-- Other utility functions
restrictMag m vec
    | abs vec > s m     = setMag (s m) vec
    | otherwise         = vec

restrictDir dir r vec
    | abs dir < s 0.01  = vec
    | otherwise         = vec'
  where lr = norm $ rotateVec rd hed
        rr = norm $ rotateVec (-rd) hed
        vec'| S rd > (acos $ hed*(norm vec))    = vec
            | cross (norm vec) hed > 0          = (abs vec)*rr
            | otherwise                         = (abs vec)*lr
        hed = norm dir
        rd = r*pi/180

-- Compute the repulsion force from a close neighbour
computeRepulsion :: Float           -- ^Distance at which a butd is crowded
                 -> (Float, Float)  -- ^Min and Max repulsion force
                 -> Vec2F           -- ^Source bird position
                 -> Vec2F           -- ^Target bird position
                 -> Vec2F           -- ^Repulsion force
computeRepulsion crowdR (s->low, s->high) myPos otherPos
    | s' mag > crowdR   = V 0 0
    | otherwise         = setMag (mag*(low-high)/(S crowdR)+high) vec
  where vec = myPos-otherPos
        mag = abs vec

-- Compute the cohesion for a bird's velocity
computeCohesion :: Float        -- ^Blending paramater
                -> Vec2F        -- ^Main vector
                -> Vec2F        -- ^Other vector
                -> Vec2F        -- ^Cohesion vector
computeCohesion (s->weight) v1 v2 = m * V (cos theta) (sin theta)
  where m       = abs v1
        theta   = (\(V x y) -> atan2 y x) $ weight*(norm v1) + (1-weight)*(norm v2)

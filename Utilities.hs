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
restrictMag m v
    | abs v > s m   = setMag (s m) v
    | otherwise     = v

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
computeRepulsion crowdR (low, high) myPos otherPos
    | s' mag > crowdR   = V 0 0
    | otherwise         = setMag (mag*(low-high)/(S crowdR)+high) vec
  where vec = (v myPos)-(v otherPos)
        mag = abs vec

-- Compute the cohesion for a bird's velocity
computeCohesion p v1 v2 = v (m*(cos theta),m*(sin theta))
  where m       = s' $ abs v1
        theta   = (\(V x y) -> atan2 y x) $ p*(norm v1) + (1-p)*(norm v2)

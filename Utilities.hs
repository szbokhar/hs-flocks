{-# LANGUAGE ViewPatterns #-}

module Utilities where

import Data.Array
import Data.List        ( foldl' )
import Graphics.Gloss   ( Point )

import qualified Data.Vector            as V

import Vector

-- |Computes all the neighbours in a given radius
neighbours :: Eq a
           => Float            -- ^Neighbourhood radius
           -> (Float, Float)   -- ^Width and height of the area
           -> (a->Point)       -- ^Position function
           -> [a]              -- ^List of items
           -> [(a,[a])]        -- ^List of item and neighbours within radius
neighbours rad (w,h) posF itemList = map findNeighbours eligibleList
  where
    -- Number of cells/slots in the x and y direction
    (xslots, yslots) = ([-w/2,-w/2+rad..w/2], [-h/2,-h/2+rad..h/2])
    -- The grid size
    sz@(gx,gy) = (length xslots-1, length yslots-1)
    -- Array of cells and contained birds, and a list of birds and corresponding cells
    (buckets,items) = foldl' placeItem
                             (listArray ((0,0),(gx+1,gy+1)) $ repeat [],[])
                             (zip [(1::Int)..] itemList)
    -- Takes a list of neighbour candidtaes and selects the true neighbours
    findNeighbours ((i,x),xs) = (x,map snd $ filter helper xs)
      where (x1,y1) = posF x
            helper (j,a) = (i /= j)
                        && (sqrt $ (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2)) < rad
              where (x2,y2) = posF a
    -- Takes the cell a bird is in and constructs a list of candidate cells
    adjacentCells (x,y) = filter
        (\(ix,iy) -> not $ ix < 1 || iy < 1 || ix > gx || iy > gy)
        [(a,b) | a<-[x-1,x,x+1], b<-[y-1,y,y+1] ]
    -- List of birds and candidate birds
    eligibleList = sndMap (concatMap (buckets!) . adjacentCells) items
    -- Constructs an array of cells and contained birds
    placeItem (gridArray,itemsAndLocations) item@((_,posF->(x,y))) =
        (accum (\ a b -> b:a) gridArray [(key,item)], (item,key):itemsAndLocations)
       where key = ( length $ takeWhile (x>) xslots
                   , length $ takeWhile (y>) yslots)

-- |Computes all the neighbours in a given radius
neighbours' :: Eq a
           => Float            -- ^Neighbourhood radius
           -> (Float, Float)   -- ^Width and height of the area
           -> (a->Point)       -- ^Position function
           -> V.Vector a       -- ^List of items
           -> V.Vector (a,[a]) -- ^List of item and neighbours within radius
neighbours' rad (w,h) posF itemList = V.fromList $ map findNeighbours eligibleList
  where
    -- Number of cells/slots in the x and y direction
    (xslots, yslots) = ([-w/2,-w/2+rad..w/2], [-h/2,-h/2+rad..h/2])
    -- The grid size
    sz@(gx,gy) = (length xslots-1, length yslots-1)
    -- Array of cells and contained birds, and a list of birds and corresponding cells
    (buckets,items) = foldl' placeItem
                             (listArray ((0,0),(gx+1,gy+1)) $ repeat [],[])
                             (zip [(1::Int)..] $ V.toList itemList)
    -- Takes a list of neighbour candidtaes and selects the true neighbours
    findNeighbours ((i,x),xs) = (x,map snd $ filter helper xs)
      where (x1,y1) = posF x
            helper (j,a) = (i /= j)
                        && (sqrt $ (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2)) < rad
              where (x2,y2) = posF a
    -- Takes the cell a bird is in and constructs a list of candidate cells
    adjacentCells (x,y) = filter
        (\(ix,iy) -> not $ ix < 1 || iy < 1 || ix > gx || iy > gy)
        [(a,b) | a<-[x-1,x,x+1], b<-[y-1,y,y+1] ]
    -- List of birds and candidate birds
    eligibleList = sndMap (concatMap (buckets!) . adjacentCells) items
    -- Constructs an array of cells and contained birds
    placeItem (gridArray,itemsAndLocations) item@((_,posF->(x,y))) =
        (accum (\ a b -> b:a) gridArray [(key,item)], (item,key):itemsAndLocations)
       where key = ( length $ takeWhile (x>) xslots
                   , length $ takeWhile (y>) yslots)

-- |Wrap the birds within the boundery
wrapBird :: (a -> Point) -> (a -> Point -> a) -> (Float, Float) -> a -> a
wrapBird posF setPosF (w,h) item = setPosF item (wrap x bx, wrap y by)
  where (x,y) = posF item
        (bx,by) = (w/2,h/2)
        wrap val bound  | abs val > bound   = val - 2 * signum val * bound
                        | otherwise         = val

-- |Wrap the birds within the boundery
wrapPos :: (Float, Float) -> Point -> Point
wrapPos (w,h) (x,y) = (wrap x bx, wrap y by)
  where (bx,by) = (w/2,h/2)
        wrap val bound  | abs val > bound   = val - 2 * signum val * bound
                        | otherwise         = val

-- |Compute the repulsion force from a close neighbour
computeRepulsion :: Float           -- ^Distance at which a bird is crowded
                 -> (Float, Float)  -- ^Min and Max repulsion force
                 -> Vec2F           -- ^Source bird position
                 -> Vec2F           -- ^Target bird position
                 -> Vec2F           -- ^Repulsion force
computeRepulsion crowdR (s->low, s->high) myPos otherPos
    | s' mag > crowdR   = V 0 0
    | otherwise         = setMag (mag * (low-high) / S crowdR + high) vec
  where vec = myPos-otherPos
        mag = abs vec

-- |Compute the cohesion for a bird's velocity
computeCohesion :: Float        -- ^Blending paramater
                -> Vec2F        -- ^Main vector
                -> Vec2F        -- ^Other vector
                -> Vec2F        -- ^Cohesion vector
computeCohesion (s->weight) v1 v2 = m * V (cos theta) (sin theta)
  where m       = abs v1
        theta   = (\(V x y) -> atan2 y x)
                $ weight * norm v1 + (1-weight) * norm v2

-- |Restrict the magnitude of a vector. Returns the original vector if it's
--  magnitude is less than the magnitude supplied. Otherwise sets the magnitude
--  of the supplied vector to the supplied magnitude
restrictMag :: Float -> Vec2F -> Vec2F
restrictMag m vec
    | abs vec > s m     = setMag (s m) vec
    | otherwise         = vec

-- |Restricts the direction of a vector
restrictDir :: Vec2F -> Float -> Vec2F -> Vec2F
restrictDir dir r vec
    | abs dir < s 0.01  = vec
    | otherwise         = vec'
  where lr = norm $ rotateVec rd hed
        rr = norm $ rotateVec (-rd) hed
        vec'| S rd > acos (hed * norm vec)      = vec
            | cross (norm vec) hed > 0          = abs vec * rr
            | otherwise                         = abs vec * lr
        hed = norm dir
        rd = r*pi/180

-- |Converts a string to an Int. Thorws an error if this is not possible
int :: String -> Int
int = read

-- View Pattern Aliases for vectors, points and scalers
v :: Point -> Vec2F
v = toVec

p :: Vec2F -> Point
p = toPoint

s :: Float -> Vec2F
s = S

s' :: Vec2F -> Float
s' (S a) = a
s' _ = error "Not a scalar"

-- |Reverse arguments of map
(|>) :: Functor f => f a -> (a -> b) -> f b
(|>) a b = fmap b a

-- |Maps a function over the second element in a list of 2-tuples
sndMap :: (a -> b) -> [(c,a)] -> [(c,b)]
sndMap f = map (\(a,b) -> (a,f b))

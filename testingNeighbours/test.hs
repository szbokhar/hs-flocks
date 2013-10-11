{-# LANGUAGE ViewPatterns #-}

import Graphics.Gloss
import qualified Data.Map as M
import Data.List
import Vector
import Control.Monad
import System.Random

v (x,y) = V x y
mode = InWindow "foo" (800,600) (30,30)

main = do
    xs <- forM [1..100] (\x -> do
        x <- randomRIO (-400,400) :: IO Float
        y <- randomRIO (-300,300) :: IO Float
        return (x,y) )
    -- neighbours' 40 (800,600) id id xs
    let pic = drawNet $ neighbours' 75 (800,600) id id xs
    display mode white pic -- (Pictures $ drawPoints xs)

sndMap f = map (\(a,b) -> (a,f b))

drawPoints = map (\(x,y) -> translate x y $ color blue $ circleSolid 3)
drawNet list = Pictures $ concatMap (\(it,xs)-> map (\a -> line [it,a]) xs) list
                  ++ drawPoints (map fst list)

{-
neighbours' :: Float             -- ^Neighbourhood radius
            -> (Float, Float)    -- ^Width and height of the area
            -> (a->Point)        -- ^Position function
            -> (a->Point)        -- ^Velocity function
            -> [a]               -- ^List of items
            -> [(a,[a])]         -- ^List of item and neighbours within radius
            -}
neighbours' rad (w,h) posF velF xs = map findNeighbours eligibleList
  where (xslots, yslots) = ([-w/2,-w/2+rad..w/2], [-h/2,-h/2+rad..h/2])
        (gx,gy) = (length xslots-1, length yslots-1)
        (buckets,items) = foldl' placeItem (M.empty,[]) xs

        findNeighbours (x,xs) = (x,filter (\a -> abs (v (posF x) - v (posF a)) < S rad) xs)

        adjacentCells (x,y) = filter (\(x,y) -> not $ x < 1 || y < 1 || x > gx || y > gy)
                                [(a,b) | a<-[x-1,x,x+1], b<-[y-1,y,y+1] ]

        eligibleList = sndMap (concatMap (\a -> M.findWithDefault [] a buckets) . adjacentCells) items

        placeItem (gridMap,itemsAndLocations) item@(posF->(x,y)) =
            (M.insertWith' (++) key [item] gridMap, (item,key):itemsAndLocations)
           where key = ( length $ takeWhile (x>) xslots
                       , length $ takeWhile (y>) yslots)

-- |Computes all the neighbours in a given radius
neighbours :: Float             -- ^Neighbourhood radius
           -> (a->Point)        -- ^Position function
           -> (a->Point)        -- ^Velocity function
           -> [a]               -- ^List of items
           -> [(a,[a])]         -- ^List of item and neighbours within radius
neighbours rad posF velF= foldl' (add []) []
  where add n [] b = [(b,n)]
        add n ((t,tn):xs) b
            | abs (p1-p2) > S rad   = (t,tn) : add n xs b
            | otherwise             = (t,b:tn) : add (t:n) xs b
          where [p1,_,p2,_] = map v [posF b, velF b, posF t, velF t]
                --_ = (pi/2) < (acos $ (norm v1) * (norm $ p2-p1))
                --_ = (pi/2) < (acos $ (norm v2) * (norm $ p1-p2))


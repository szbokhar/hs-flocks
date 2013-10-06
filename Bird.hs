{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Bird where

import Control.DeepSeq
import Control.Monad
import Control.Applicative              ( (<$>) )
import Data.List                        ( foldl' )
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

import Utilities
import Vector

-------------------------------------------------------------------------------
----------------------------- Class Definitions -------------------------------

-- |Class for all things that can be drawn
class Drawable a where
    draw :: a -> Picture

-- |A flock of some type of creature
data Flock a =
     Flock  { population    :: [a]              -- ^List of population members
            , target        :: Point            -- ^Point all members are drawn
            , field         :: (Float, Float)   -- ^Game field
            , neighbourhood :: (Float, Float)   -- ^Crowd and vision radius
            }
  deriving (Read, Show, Eq)

-- |A bird creature
data Bird =
     Bird   { position  :: Point                -- ^Position of the bird
            , velocity  :: Point                -- ^Velocity vector of the bird
            , size      :: Float                -- ^Size to draw the bird
            , maxspeed  :: Float                -- ^The max speed of the bird
            , turnRange :: Float                -- ^Max allowable turning angle
            }
  deriving (Read, Show, Eq)


-------------------------------------------------------------------------------
----------------------- Instance Definitions for NFData -----------------------

-- |Allow a Bird to be fully evaluated with deepseq
instance NFData Bird where
    rnf (Bird {..}) = foldl' seq ()
        $ [rnf position, rnf velocity, rnf size, rnf maxspeed, rnf turnRange]

-- |Allow a Flock of NFData to be fully evaluated with deepseq
instance NFData a => NFData (Flock a) where
    rnf (Flock {..}) = foldl' seq ()
        $ [rnf population, rnf target, rnf field, rnf neighbourhood]


-------------------------------------------------------------------------------
----------------------- Instance Defintions for Drawable ----------------------

-- |Allows a bird to be drawn
instance Drawable Bird where
    draw (Bird {position=(x,y), velocity=(vx,vy), size=sz}) =
        drawBird (x,y,vx,vy,sz)

-- |Allows a flock of any type of drawable cerature to be drawn
instance Drawable a => Drawable (Flock a) where
    draw (Flock {target=(tx,ty), field=(w,h), population=pop}) =
        Pictures $ (rectangleWire w h)
                 : (Translate tx ty $ Color red $ circleSolid 5)
                 : (map draw pop)

-- |Barebones bird drawing function
drawBird :: (Float, Float, Float, Float, Float) -> Picture
drawBird (x,y,vx,vy,sz) =
      Pictures [ Translate x y $ Color blue $ Polygon [ p1, p2, p3 ] ]
  where h = sz/2
        t = atan2 vy vx
        t2 = 120*pi/180
        p1 = (sz*cos(t),sz*sin(t))
        p2 = (h*cos(t-t2),h*sin(t-t2))
        p3 = (h*cos(t+t2),h*sin(t+t2))


-------------------------------------------------------------------------------
-------------------------- Flock Dynamics Functions ---------------------------

-- |Affect the flock based on user input
react :: Event -> Flock Bird -> Flock Bird
react (EventMotion pos) flock = flock { target = pos }
react _ fl = fl

-- |Update the flock for each timestep
update :: Float -> Flock Bird -> IO (Flock Bird)
update _ f@(Flock {neighbourhood=(crowdR,visionR), ..}) = do
    pop' <-  map (wrapBirds field)                      -- Wrap on screen
         <$> mapM (move target crowdR) closeBirds       -- Move all birds
    return $ f { population = pop' }
  where closeBirds = neighbours visionR population      -- Find neighbours

-- |Move every bord in the flock towards the point
move :: Point           -- Target point that bird is drawn to
     -> Float           -- Radius at which bird feels crowded
     -> (Bird, [Bird])  -- Bird to update and a list of neighbours
     -> IO Bird         -- Updated bird
move (v->goal) crowdR (bird,birds) = do
    return bird { position = toPoint pos'       -- Update the position
                , velocity = toPoint vel' }     -- and velocity of the bird
  where
    -- Parts of a bird
    Bird { position = v->pos
         , velocity = v->vel
         , maxspeed = maxspd
         , turnRange = r} = bird

    -- New position and velocity
    pos' = (pos + vel')
    vel' = computeCohesion 0.9 fvel nvel

    -- Velocity from resultant forces
    fvel = restrictDir vel r $
           restrictMag maxspd (0.5*(vel + (
           restrictMag maxspd $ vel+totalForce )))
    -- Average velocity from neighbour birds
    nvel = (foldl' (\a b -> a+(v$velocity b)) fvel birds)/(1+numNeighbours)

    -- Total force that affects the bird's motion
    totalForce = neighbourForce birds + crowdForce + goalForce
    -- Foce drawing the bird towards the target (mouse)
    goalForce = let _ = abs v; v = (goal-pos) in setMag 0.2 v
    -- Force that pushes the bird away from close neighbours
    crowdForce = sum $ map ((computeRepulsion crowdR (0,5) (p pos)) . position) birds
    -- Fore that draws the bird to the average position of all it's neighbours
    neighbourForce [] = V 0 0
    neighbourForce b  = setMag 1 $ ((sum $ map (v . position) b) / numNeighbours) - pos

    -- Number of neighbours the bird has
    numNeighbours = fromIntegral $ length birds

-- |Computes all the neighbours in a given radius for each bird
neighbours :: Float             -- Neighbourhood radius
           -> [Bird]            -- List of birds
           -> [(Bird,[Bird])]   -- List of birds and neighbours within radius
neighbours rad = foldl' (addBird []) []
  where addBird :: [Bird] -> [(Bird,[Bird])] -> Bird -> [(Bird,[Bird])]
        addBird n [] b = [(b,n)]
        addBird n ((t,tn):xs) b
            | abs (p1-p2) > S rad   = (t,tn) : (addBird n xs b)
            | otherwise             = (t,b:tn) : (addBird (t:n) xs b)
          where Bird { position = v->p2, velocity = v->v2 } = t
                Bird { position = v->p1, velocity = v->v1 } = b
                _ = (pi/2) < (acos $ (norm v1) * (norm $ p2-p1))
                _ = (pi/2) < (acos $ (norm v2) * (norm $ p1-p2))

-- |Wrap the birds within the boundery
wrapBirds :: (Float, Float) -> Bird -> Bird
wrapBirds (w,h) bird = bird { position = (wrap x bx, wrap y by) }
  where (x,y) = position bird
        (bx,by) = (w/2,h/2)
        wrap val bound  | abs val > bound   = val - 2*(signum val)*bound
                        | otherwise         = val


-------------------------------------------------------------------------------
--------------------------- Other Handy Functions -----------------------------

-- |Converts a flock to a consice writable form
writeFlock :: Flock Bird -> [(Float, Float, Float, Float, Float)]
writeFlock (Flock { population = birds }) = map makeTup birds
  where makeTup (Bird (x,y) (vx,vy) s _ _) = (x,y,vx,vy,s)

-- |Creates a random flock of birds
makeFlock :: Float -> Float -> (Float, Float) -> Float -> Int -> IO (Flock Bird)
makeFlock width height (lspd, tspd) fact n = do
    birds <- forM [1..n] (\_ -> do
        x <- randomRIO (-width/2,width/2)       -- Random x position
        y <- randomRIO (-height/2,height/2)     -- Random y position
        dir <- randomRIO (0,2*pi)               -- Direstion of bird
        spd <- randomRIO (lspd,tspd)            -- Speed of bird
        return Bird { position = (x,y)
                    , velocity = (spd*cos dir,spd*sin dir)
                    , size = (fact*spd)
                    , maxspeed = spd
                    , turnRange = 10 }
        )
    return Flock { population = birds
                 , target = (20,20)
                 , field = (width,height)
                 , neighbourhood = (50,75) }

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Animal where

import Control.DeepSeq                  ( NFData(..), deepseq )
import Control.Monad
import Data.List                        ( foldl' )
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

import Simulate
import Utilities    ( v, p, computeCohesion, computeRepulsion
                    , restrictDir, restrictMag, neighbours )
import Vector       ( Vec2F(..), setMag )
-------------------------------------------------------------------------------
----------------------------- Datatype Definitions ----------------------------

-- |A flock of some type of creature
data Flock =
     Flock  { population    :: [Bird]           -- ^List of population members
            , target        :: Point            -- ^Point all members are drawn
            , constants     :: DynamicsConstants-- ^Dynamic constants
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

data DynamicsConstants =
     DC { targetForce       :: Float
        , neighbourForce    :: Float
        , crowdForce        :: Float
        , field             :: (Float, Float)   -- ^Game field
        , neighbourhood     :: (Float, Float)   -- ^Crowd and vision radius
        }
  deriving (Read, Show, Eq)

-------------------------------------------------------------------------------
----------------------- Instance Definitions for NFData -----------------------

-- |Allow a Bird to be fully evaluated with deepseq
instance NFData Bird where
    rnf (Bird {..}) = foldl' seq ()
        [rnf position, rnf velocity, rnf size, rnf maxspeed, rnf turnRange]

-- |Allow a Flock of NFData to be fully evaluated with deepseq
instance NFData Flock where
    rnf (Flock {..}) = foldl' seq ()
        [rnf population, rnf target, rnf field, rnf neighbourhood]


-------------------------------------------------------------------------------
----------------------- Instance Defintions for Drawable ----------------------

-- |Allows a bird to be drawn
instance Drawable Bird where
    draw (Bird {position=(x,y), velocity=(vx,vy), size=sz}) =
        drawBird (x,y,vx,vy,sz)

-- |Allows a flock of any type of drawable cerature to be drawn
instance Drawable Flock where
    draw (Flock {target=(tx,ty), population=pop, constants=DC {field=(w,h)}}) =
        Pictures $ (rectangleWire w h)
                 : (Translate tx ty $ Color red $ circleSolid 5)
                 : (map draw pop)

-- |Allows a flock to be simulated
instance Simulate Flock where
    -- |Update the target position on mouse movement
    react (EventMotion pos) flock = flock { target = pos }
    react _ fl = fl

    -- |No non-IO version of update
    update _ _ = error "Not implemented"

    -- |Update the flock by one timestep
    updateIO _ f@(Flock {..}) = do
        pop' <-  mapM move' closeBirds
        return $ f { population = pop' }
      where closeBirds = neighbours visionR position velocity population
            move' = moveBird target constants
            DC { neighbourhood=(_,visionR) } = constants

    -- |Converts a flock to a consice writable form
    toWritableString (Flock { population = birds }) =
        show $ map makeTup birds
      where makeTup (Bird (x,y) (vx,vy) s _ _) = (x,y,vx,vy,s)

    -- |Renders a list of writable forms to a list of pictures
    renderStringIO _ xs = do
        forM_ (zip [1..] parsedList) $ (\ (i,x) -> do
            x `deepseq` (putStrLn $ "Processing " ++ (show $ 100*i/n)) )
        return $ map (Pictures . (map drawBird)) parsedList
      where parsedList = map read xs :: [[(Float, Float, Float, Float, Float)]]
            n = fromIntegral $ length xs

    -- |Quick default for a flock
    defaultSim = makeFlock (800,600) (4,9) 1 200 0.2 2 5


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

-- |Move every bord in the flock towards the point
moveBird :: Point                   -- ^Target point that bird is drawn to
     -> DynamicsConstants       -- ^Dynamics Constants
     -> (Bird, [Bird])          -- ^Bird to update and a list of neighbours
     -> IO Bird                 -- ^Updated bird
moveBird (v->goal) consts (bird,birds) = do
    return bird { position = p pos'       -- Update the position
                , velocity = p vel' }     -- and velocity of the bird
  where
    -- Convienent identifiers
    Bird { position = posP
         , velocity = velP
         , maxspeed = maxspd
         , turnRange = r } = bird
    DC   { neighbourhood = (crowdR, visionR)
         , targetForce = tF
         , neighbourForce = nF
         , crowdForce = cF
         , field = (w,h) } = consts
    pos = v posP
    vel = v velP

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
    totalForce = neighbourForce birds + crowdForce + goalForce + borderForce
               - (0.05*vel) - (V 0 1)
    -- Foce drawing the bird towards the target (mouse)
    goalForce = setMag (S tF) (goal-pos)
    -- Force that pushes the bird away from close neighbours
    crowdForce =
        sum $ map ((computeRepulsion crowdR (0,cF) pos) . v . position) birds
    -- Force that draws the bird to the average position of all it's neighbours
    neighbourForce [] = V 0 0
    neighbourForce b  =
        setMag (S nF) $ ((sum $ map (v . position) b) / numNeighbours) - pos
    -- Force of repulsion by border
    borderForce = sum $ map (computeRepulsion visionR (0,cF*3) pos) locs
      where V xx yy = pos
            locs = [V xx (-h/2), V xx (h/2), V (-w/2) yy, V (w/2) yy]

    -- Number of neighbours the bird has
    numNeighbours = fromIntegral $ length birds

-------------------------------------------------------------------------------
--------------------------- Other Handy Functions -----------------------------

-- |Creates a random flock of birds
makeFlock :: (Float, Float)     -- ^Width and height of the field
          -> (Float, Float)     -- ^Smallest and largest max speeds for birds
          -> Float              -- ^Scalaing factor of bird size
          -> Int                -- ^Number of birds
          -> Float              -- ^Target Force
          -> Float              -- ^Neighbour Force
          -> Float              -- ^Crowd Force
          -> IO Flock           -- ^Randomized Flock of birds
makeFlock (width, height) (lspd, tspd) fact n f1 f2 f3= do
    birds <- forM [1..n] (\_ -> do
        x <- randomRIO (-width/2,width/2)       -- Random x position
        y <- randomRIO (-height/2,height/2)     -- Random y position
        dir <- randomRIO (0,2*pi)               -- Direstion of bird
        spd <- randomRIO (lspd,tspd)            -- Speed of bird
        return Bird { position = (x,y)
                    , velocity = (spd*cos dir,spd*sin dir)
                    , size = (fact*spd)
                    , maxspeed = spd
                    , turnRange = 10
                    }
        )
    return Flock { population = birds
                 , target = (20,20)
                 , constants =
                    DC { field = (width,height)
                       , neighbourhood = (50,75)
                       , targetForce = f1
                       , neighbourForce = f2
                       , crowdForce = f3
                       }
                 }

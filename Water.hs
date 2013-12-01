{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Water where

import Control.DeepSeq                  ( NFData(..), deepseq )
import Control.Monad
import Data.List                        ( foldl' )
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

import qualified Data.Vector            as V

import Simulate
import Utilities
import Vector       ( Vec2F(..), setMag )

-------------------------------------------------------------------------------
----------------------------- Datatype Definitions ----------------------------

-- |A flock of some type of creature
data Fluid = Fluid
            { population    :: V.Vector WaterP  -- ^List of population members
            , target        :: !Point            -- ^Point all members are drawn
            , constants     :: !DynamicsConstants-- ^Dynamic constants
            }
  deriving (Read, Show, Eq)

-- |A bird creature
data WaterP = WaterP
            { position  :: !Point                -- ^Position of the bird
            , velocity  :: !Point                -- ^Velocity vector of the bird
            , size      :: !Float                -- ^Size to draw the bird
            , maxspeed  :: !Float                -- ^The max speed of the bird
            }
  deriving (Read, Show, Eq)

-- |Datatype to contain simulation values
data DynamicsConstants =
     DC { targetForce       :: !Float           -- ^Mouse position
        , neighbourForce    :: !Float           -- ^Force from neighbour birds
        , crowdForce        :: !Float           -- ^Force from crowded birds
        , damp              :: !Float           -- ^Damping from velocity
        , gravity           :: !Float           -- ^Damping from velocity
        , field             :: !(Float, Float)  -- ^Game field
        , neighbourhood     :: !(Float, Float)  -- ^Crowd and vision radius
        }
  deriving (Read, Show, Eq)

-------------------------------------------------------------------------------
----------------------- Instance Definitions for NFData -----------------------

-- |Allow a Bird to be fully evaluated with deepseq
instance NFData WaterP where
    rnf (WaterP {..}) = foldl' seq ()
        [rnf position, rnf velocity, rnf size, rnf maxspeed]

-- |Allow a Flock of NFData to be fully evaluated with deepseq
instance NFData Fluid where
    rnf (Fluid {..}) = foldl' seq ()
        [rnf population, rnf target, rnf field, rnf neighbourhood]


-------------------------------------------------------------------------------
----------------------- Instance Defintions for Drawable ----------------------

-- |Allows a bird to be drawn
instance Drawable WaterP where
    draw (WaterP {position=v->pos@(V x y), velocity=v->vel}) =
        Pictures $ (Line $ map p [pos+0.5*vel, pos-0.5*vel])
                 : [Translate x y $ Color blue $ Circle 1]

-- |Allows a flock of any type of drawable cerature to be drawn
instance Drawable Fluid where
    draw (Fluid {target=(tx,ty), population=pop, constants=DC {field=(w,h)}}) =
        Pictures $ rectangleWire w h
                 : Translate tx ty (Color red $ circleSolid 5)
                 : V.toList (V.map draw pop)


-------------------------------------------------------------------------------
----------------------- Instance Defintion for Simulate -----------------------

-- |Allows a flock to be simulated
instance Simulate Fluid where
    -- |Update the target position on mouse movement
    react (EventMotion pos) fl = fl { target = pos }
    react _ fl = fl

    -- |No non-IO version of update
    update _ _ = error "Not implemented"

    -- |Quick default for a flock
    defaultSim = makeFluid (800,600) (4,9) 1 200 0.2 2 5 0.1 0.5 (50, 75)

    -- |Update the flock by one timestep
    updateIO _ f@(Fluid {..}) = do
        pop' <- V.mapM (moveWaterP target constants closeParts) closeParts
        return $ f { population = pop' }
      where closeParts = neighbours' visionR field position population
            DC { neighbourhood=(_,visionR), field=field} = constants

    -- |Converts a flock to a consice writable form
    toWritableString (Fluid { population = parts }) =
        show $ map makeTup $ V.toList parts
      where makeTup (WaterP { position=(x,y), size=sz}) = (x,y,sz)

    -- |Renders a list of writable forms to a list of pictures
    renderStringIO _ xs = do
        forM_ (zip [(1::Double)..] parsedList) (\ (i,x) ->
            x `deepseq` putStrLn $ "Processing " ++ show (100*i/n) )
        return $ map (Pictures . map drawCircle) parsedList
      where parsedList = map read xs :: [[(Float, Float, Float)]]
            n = fromIntegral $ length xs


-- |Barebones bird drawing function
drawCircle :: (Float, Float, Float) -> Picture
drawCircle (x,y,sz) = color blue $ translate x y $ circle 3

-------------------------------------------------------------------------------
-------------------------- Flock Dynamics Functions ---------------------------

-- |Move every bord in the flock towards the point
moveWaterP :: Point                   -- ^Target point that part is drawn to
           -> DynamicsConstants       -- ^Dynamics Constants
           -> V.Vector (WaterP,[WaterP])
           -> (WaterP, [WaterP])      -- ^Bird to update and a list of neighbours
           -> IO WaterP               -- ^Updated part
moveWaterP (v->goal) consts nlist (part,nParts) =
    return $ keepIn $ part { position = p pos', velocity = p vel' }
  where
    -- Convienent identifiers
    WaterP { position = posP, velocity = velP, maxspeed = maxspd }
        = part
    DC   { neighbourhood = (crowdR, visionR), targetForce = tF
         , neighbourForce = nF, crowdForce = cF, field = (w,h)
         , damp=damp, gravity=grav } = consts
    (pos,vel) = (v posP, v velP)

    -- New position and velocity
    (pos',vel') = (pos + vel', computeCohesion 0.7 fvel nvel)
    -- Velocity from resultant forces
    fvel = restrictMag maxspd
           (0.5 * (vel + restrictMag maxspd (vel + totalForce)))
    -- Average velocity from neighbour nParts
    nvel = foldl' (\a b -> a + v (velocity b)) fvel nParts / (1+numNeighbours)

    -- Total force that affects the part's motion
    totalForce = nForce nParts + sum crowdForce + borderForce + goalForce
               + gravForce + dampForce
    -- Foce drawing the part towards the target (mouse)
    goalForce = setMag (S tF) (goal-pos)
    -- Force that pushes the part away from close neighbours
    crowdForce = map (computeRepulsion crowdR (0,cF) pos . v . position) nParts
    -- Force that draws the part to the average position of all it's neighbours
    nForce [] = V 0 0
    nForce b  = setMag (S nF) $ sum (map (v . position) b) / numNeighbours - pos
    -- Force of repulsion by border
    borderForce = sum $ map (computeRepulsion visionR (0,cF) pos) locs
      where V xx yy = pos
            locs = [V xx (-h/2), V xx (h/2), V (-w/2) yy, V (w/2) yy]
    -- Damping force
    dampForce = - (S damp) * vel
    -- Gravity Force
    gravForce = - V 0 grav

    -- Border
    keepIn wp@(WaterP {position = (px, py), velocity = (vx,vy)})
        = wp { position = (px',py'), velocity = (vx', vy') }
      where
        (px',vx') = wrap px vx (w/2)
        (py',vy') = wrap py vy (h/2)
        wrap pval vval bound
            | pval > bound      = (bound-1,-0.5*vval)
            | pval < -bound     = (-bound+1,-0.5*vval)
            | otherwise         = (pval,vval)
    -- Number of neighbours the part has
    numNeighbours = fromIntegral $ length nParts

-------------------------------------------------------------------------------
--------------------------- Other Handy Functions -----------------------------

-- |Creates a random flock of birds
makeFluid :: (Float, Float)     -- ^Width and height of the field
          -> (Float, Float)     -- ^Smallest and largest max speeds for birds
          -> Float              -- ^Scalaing factor of bird size
          -> Int                -- ^Number of birds
          -> Float              -- ^Target Force
          -> Float              -- ^Neighbour Force
          -> Float              -- ^Crowd Force
          -> Float              -- ^Damp Force
          -> Float              -- ^Gravity Force
          -> (Float, Float)     -- ^Neighbourhood radii
          -> IO Fluid           -- ^Randomized Flock of birds
makeFluid (width, height) (lspd, tspd) fact n f1 f2 f3 f4 f5 hood = do
    birds <- forM [1..n] (\_ -> do
        x <- randomRIO (-width/2,width/2)       -- Random x position
        y <- randomRIO (-height/2,height/2)     -- Random y position
        dir <- randomRIO (0,2*pi)               -- Direstion of bird
        spd <- randomRIO (lspd,tspd)            -- Speed of bird
        return WaterP { position = (x,y)
                    , velocity = (spd*cos dir,spd*sin dir)
                    , size = fact*spd
                    , maxspeed = tspd
                    }
        )
    return Fluid { population = V.fromList birds
                 , target = (20,20)
                 , constants =
                    DC { field = (width,height)
                       , neighbourhood = hood
                       , targetForce = f1
                       , damp = f4
                       , gravity = f5
                       , neighbourForce = f2
                       , crowdForce = f3
                       }
                 }

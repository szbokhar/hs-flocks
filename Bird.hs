{-# LANGUAGE ViewPatterns #-}

module Bird where

import Control.Monad
import Control.Applicative              ( (<$>) )
import Data.List                        ( foldl' )
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

import Vector

-- |Class for all things that can be drawn
class Drawable a where
    draw :: a -> Picture

-- |A flock of some type of creature
data Flock a =
     Flock { population       :: [a]            -- List of population members
           , target           :: Point          -- Point all members are drawn to
           , field            :: (Float, Float) -- Game field
           , neighbourhood    :: (Float, Float) -- Crowd and vision radius
           }
  deriving (Read, Show, Eq)

-- |A bird creature
data Bird = Bird { position :: Point
                 , velocity :: Point
                 , size :: Float
                 , maxspeed :: Float
                 , turnRange :: Float }
  deriving (Read, Show, Eq)

-- |Allows a bird to be drawn
instance Drawable Bird where
    draw (Bird (x,y) (vx,vy) sz _ _) =
          Pictures [ Translate x y $ Color blue $ Polygon [ p1, p2, p3 ] ]
      where h = sz/2
            t = atan2 vy vx
            t2 = 120*pi/180
            p1 = (sz*cos(t),sz*sin(t))
            p2 = (h*cos(t-t2),h*sin(t-t2))
            p3 = (h*cos(t+t2),h*sin(t+t2))

-- |Allows a flock of any type of drawable cerature to be drawn
instance Drawable a => Drawable (Flock a) where
    draw (Flock pop (tx,ty) (w,h) _) =
        Pictures $ (rectangleWire w h)
                 : (Translate tx ty $ Color red $ circleSolid 5)
                 : (map draw pop)

-- |Creates a random flock of birds
makeFlock :: Float -> Float -> (Float, Float) -> Float -> Int -> IO (Flock Bird)
makeFlock width height (lspd, tspd) fact n = do
    birds <- forM [1..n] (\_ -> do
        x <- randomRIO (-width/2,width/2)       -- Random x position
        y <- randomRIO (-height/2,height/2)     -- Random y position
        dir <- randomRIO (0,2*pi)               -- Direstion of bird
        spd <- randomRIO (lspd,tspd)            -- Speed of bird
        return (Bird (x,y) (spd*cos dir,spd*sin dir) (fact*spd) spd 10) )
    return (Flock birds (20,20) (width,height) (50,75))

-- |Affect the flock based on user input
react :: Event -> Flock Bird -> Flock Bird
react (EventMotion pos) flock = flock { target = pos }
react _ fl = fl

-- |Update the flock for each timestep
update :: Float -> Flock Bird -> IO (Flock Bird)
update time f@(Flock pop tar dim (crowdR,visionR)) = do
    pop' <-  map (wrapBirds dim)                -- Wrap on screen
         <$> mapM (move tar crowdR) closeBirds  -- Move all birds
    return $ f { population = pop' }
  where closeBirds = neighbours visionR pop     -- Find neighbours

-- |Move every bord in the flock towards the point
move :: Point           -- Target point that bird is drawn to
     -> Float           -- Radius at which bird feels crowded
     -> (Bird, [Bird])  -- Bird to update and a list of neighbours
     -> IO Bird         -- Updated bird
move (v->goal) crowdR (bird@(Bird (v->pos) (v->vel) _ maxspd r),birds) = do
    return bird { position = toPoint pos'       -- Update the position
                , velocity = toPoint vel' }     -- and velocity of the bird
  where
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
    totalForce = neighbourForce+crowdForce+goalForce
    -- Foce drawing the bird towards the target (mouse)
    goalForce = let m = abs v; v = (goal-pos) in setMag 0 v
    -- Force that pushes the bird away from close neighbours
    crowdForce = sum $ map (computeRepulsion (0,5) pos) birds
    -- Fore that draws the bird to the average position of all it's neighbours
    neighbourForce
        | null birds    = V 0 0
        | otherwise     =
            setMag 1 $ ((sum $ map (v . position) birds) / numNeighbours) - pos

    -- Number of neighbours the bird has
    numNeighbours = fromIntegral $ length birds
    -- Compute the repulsion force from a close neighbour
    computeRepulsion (low, high) p bird
        | s' mag > crowdR   = V 0 0
        | otherwise         = setMag (mag*(low-high)/(S crowdR)+high) vec
      where vec = pos-(v $ position bird)
            mag = abs vec
    -- Compute the cohesion for a bird's velocity
    computeCohesion p v1 v2 = v (m*(cos theta),m*(sin theta))
      where m       = s' $ abs v1
            theta   = (\(V x y) -> atan2 y x) $ p*(norm v1) + (1-p)*(norm v2)

-- |Computes all the neighbours in a given radius for each bird
neighbours :: Float             -- Neighbourhood radius
           -> [Bird]            -- List of birds
           -> [(Bird,[Bird])]   -- List of birds and neighbours within radius
neighbours rad = foldl' (addBird []) []
  where addBird :: [Bird] -> [(Bird,[Bird])] -> Bird -> [(Bird,[Bird])]
        addBird n [] b = [(b,n)]
        addBird n ((t@(Bird (v->p2) (v->v2) _ _ _),tn):xs) b@(Bird (v->p1) (v->v1) _ _ _)
            | abs (p1-p2) > S rad   = (t,tn) : (addBird n xs b)
            | otherwise             = (t,b:tn) : (addBird (t:n) xs b)
          where front1 = (pi/2) < (acos $ (norm v1) * (norm $ p2-p1))
                front2 = (pi/2) < (acos $ (norm v2) * (norm $ p1-p2))

-- |Wrap the birds within the boundery
wrapBirds :: (Float, Float) -> Bird -> Bird
wrapBirds (w,h) bird = bird { position = (wrap x bx, wrap y by) }
  where (x,y) = position bird
        (bx,by) = (w/2,h/2)
        wrap val bound  | abs val > bound   = val - 2*(signum val)*bound
                        | otherwise         = val

-- View Pattern Aliases
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

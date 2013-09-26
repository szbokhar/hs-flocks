{-# LANGUAGE ViewPatterns #-}

module Bird
    ( makeFlock, draw, react, update )
where

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
data Flock a = Flock { population   :: [a]
                     , target       :: Point
                     , field        :: (Float, Float) }
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
    draw (Flock pop (tx,ty) (w,h)) =
        Pictures $ (rectangleWire w h)
                 : (Translate tx ty $ Color red $ circleSolid 5)
                 : (map draw pop)

-- |Creates a random flock of birds
makeFlock :: Float -> Float -> (Float, Float) -> Float -> Int -> IO (Flock Bird)
makeFlock width height (lspd, tspd) fact n = do
    birds <- forM [1..n] (\_ -> do
        x <- randomRIO (-width/2,width/2)
        y <- randomRIO (-height/2,height/2)
        dir <- randomRIO (0,2*pi)
        spd <- randomRIO (lspd,tspd)
        return (Bird (x,y) (spd*cos dir,spd*sin dir) (fact*spd) spd 10) )
    return (Flock birds (20,20) (width,height))

-- |Affect the flock based on user input
react :: Event -> Flock Bird -> Flock Bird
react (EventMotion pos) flock = flock { target = pos }
react _ fl = fl

-- |Update the flock for each timestep
update :: Float -> Flock Bird -> IO (Flock Bird)
update time f@(Flock pop tar dim) = do
    pop' <-  map (wrapBirds dim)
         <$> mapM (move tar) closeBirds
    return $ f { population = pop' }
  where closeBirds = neighbours 50 pop

-- |Move every bord in the flock towards the point
move :: Point -> (Bird, [Bird]) -> IO Bird
move (v->goal) (bird@(Bird (v->pos) (v->vel) _ maxspd r),birds) = do
    return bird { position = toPoint pos'
                , velocity = toPoint vel' }
  where goalForce = setMag 0.25 $ goal - pos
        crowdForce = sum
                   $ map ((\f -> let m= (-3/50)*(abs f)+3 in setMag m f)
                        . (pos-)
                        . v
                        . position) birds
        totalForce = crowdForce + goalForce
        fvel = restrictDir vel r $
               restrictMag maxspd (0.5*(vel + (
               restrictMag maxspd $ vel+totalForce )))
        nvel = (foldl' (\a b -> a + (v $ velocity b)) fvel birds)
             / (1 + (fromIntegral . length) birds)
        vel' = let p=0.9 in p*fvel + (1-p)*nvel
        pos' = (pos + vel')

-- |Computes all the neighbours in a given radius for each bird
neighbours :: Float -> [Bird] -> [(Bird,[Bird])]
neighbours rad = foldl' (addBird []) []
  where addBird :: [Bird] -> [(Bird,[Bird])] -> Bird -> [(Bird,[Bird])]
        addBird n [] b = [(b,n)]
        addBird n ((t@(Bird (v->p2) _ _ _ _),tn):xs) b@(Bird (v->p1) _ _ _ _)
            | abs (p1-p2) > S rad = (t,tn) : (addBird n xs b)
            | otherwise           = (t,b:tn) : (addBird (t:n) xs b)

-- |Wrap the birds
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

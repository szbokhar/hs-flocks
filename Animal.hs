{-# LANGUAGE RecordWildCards #-}

module Animal where

import Control.DeepSeq
import Data.List                        ( foldl' )
import Graphics.Gloss
-------------------------------------------------------------------------------
----------------------------- Datatype Definitions -------------------------------

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

-- |Class for all things that can be drawn
class Drawable a where
    draw :: a -> Picture

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

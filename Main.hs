module Main where

import Control.Applicative                  ( (<$>) )
import Graphics.Gloss.Interface.IO.Game
import System.Environment                   ( getArgs )

import Bird ( makeFlock, draw, react, update)

mode = InWindow "Boids" (800,600) (20,20)

main = do
    count <- (\a -> if null a then 4 else head a) <$> map int <$> getArgs
    initFlock <- makeFlock 800 600 (4,9) 1 count
    playIO mode white 30 initFlock
        (return . draw)
        (\a b -> return $ react a b)
        update

int = read :: String -> Int

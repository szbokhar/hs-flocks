module Simulate where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

-- |Class for all things that can be drawn
class Drawable a where
    draw :: a -> Picture

class (Drawable a, Read a, Show a) => Simulate a where
    react :: Event -> a -> a
    react _ a = a

    update :: Float -> a -> a
    update _ a = a

    reactIO :: Event -> a -> IO a
    reactIO a b = return $ react a b

    updateIO :: Float -> a -> IO a
    updateIO a b = return $ update a b

    toWritableString :: a -> String
    toWritableString = show

    renderStringIO :: a -> [String] -> IO [Picture]
    renderStringIO _ _ = return [Blank]

    defaultSim :: IO a

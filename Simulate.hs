module Simulate where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

-- |Class for all things that can be drawn
class Drawable a where
    draw :: a -> Picture

-- |Class that makes a class simulatable
class (Drawable a, Read a, Show a) => Simulate a where
    -- |React datatype to user input
    react :: Event -> a -> a
    react _ a = a

    -- |Update datatype by one step
    update :: Float -> a -> a
    update _ a = a

    -- |IO version of react
    reactIO :: Event -> a -> IO a
    reactIO a b = return $ react a b

    -- |IO version of update
    updateIO :: Float -> a -> IO a
    updateIO a b = return $ update a b

    -- |Converts datatype to a writable string
    toWritableString :: a -> String
    toWritableString = show

    -- |Renders a written string to a list of pictures
    renderStringIO :: a -> [String] -> IO [Picture]
    renderStringIO _ _ = return [Blank]

    -- |A default for the datatype
    defaultSim :: IO a

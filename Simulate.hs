{-# LANGUAGE ExistentialQuantification #-}
module Simulate where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

-- |Class for all things that can be drawn
class Drawable a where
    draw :: a -> Picture

-- |Class that makes a type simulatable
class (Drawable a, Show a) => Simulate a where
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

-- |Container for simulation types
data Sim = forall a. (Drawable a, Show a, Simulate a) => Sim a

-- |Show declaration for Sim
instance Show Sim where
    show (Sim a) = show a

-- |Drawable declaration for Sim
instance Drawable Sim where
    draw (Sim a) = draw a

-- |Simulate declaration for Sim
instance Simulate Sim where
    react e (Sim a) = Sim (react e a)
    update n (Sim a) = Sim (update n a)
    reactIO e (Sim a) = do b <- reactIO e a
                           return (Sim b)
    updateIO n (Sim a) = do b <- updateIO n a
                            return (Sim b)
    toWritableString (Sim a) = toWritableString a
    renderStringIO (Sim a) xs = renderStringIO a xs
    defaultSim = undefined

module Simulate where

-- |Class for all things that can be drawn
class (Read a, Show a) => Simulate a where
    update :: Float -> a -> a
    update _ a = a

    updateIO :: Float -> a -> IO a
    updateIO a b = return $ update a b

    toWritableString :: a -> String
    toWritableString = show

    defaultSim :: IO a

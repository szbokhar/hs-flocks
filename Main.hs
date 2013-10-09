module Main where

import Control.Monad                            ( foldM_  )
import Data.Maybe                               ( isNothing, fromJust )
import Graphics.Gloss.Interface.IO.Game         ( playIO )
import Graphics.Gloss.Interface.Pure.Simulate   ( simulate, white
                                                , Display( InWindow ) )
import System.Environment                       ( getArgs )

import qualified System.IO as F

import Animal       ( makeFlock )
import Simulate     ( Drawable(..), Simulate(..) )
import Utilities    ( int, (|>) )

mode = InWindow "Boids" (800,600) (20,20)

-- |Main function
main = do
    config <- getArgs |> parseArguments
    initFlock <- makeFlock (800,600) (4,20) 0.5 (birdCount config)
                           0.1 3 5

    case simType config of
        Live            -> runLiveSimulation initFlock
        WriteToFile     -> writeSimulationToFile config initFlock
        ReadFromFile    -> readSimulation config initFlock
        Help            -> showHelpMessage
  where
    runLiveSimulation initFlock =
        playIO mode white 30 initFlock
            (return . draw)
            (\a b -> return $ react a b)
            updateIO

    writeSimulationToFile config initSim = do
        -- Find writing target
        target <- if isNothing (file config)
                  then return F.stdout
                  else F.openFile (fromJust (file config)) F.WriteMode
        -- Run simulation for specified number of setps
        foldM_ (\fl i -> do
            putStrLn $ "Computing frame " ++ show i
            F.hPutStrLn target (toWritableString fl)
            updateIO 0 fl
            ) initSim [1..(simulationSteps config)]
        -- Close file
        F.hClose target

    readSimulation config dummy = do
        -- Find reading target
        target <- if isNothing (file config)
                 then return F.stdin
                 else F.openFile (fromJust (file config)) F.ReadMode

        -- Parse input source into list of simulation steps
        simData <- F.hGetContents target |> lines
        simDataPictures <- renderStringIO dummy simData

        -- Play simulation
        simulate mode white 30 simDataPictures
            (\(x:_) -> x)
            (\_ _ (x:xs) -> if null xs then [x] else xs)
        -- Close source
        F.hClose target

    showHelpMessage = putStrLn $
            "FlockSimulation\n"
         ++ "Options: -n count  - number of birds (default 4)\n"
         ++ "         -l        - run live simulation (default)\n"
         ++ "         -o        - write simulation to stdout\n"
         ++ "         -i        - display simulation read in from stdin\n"
         ++ "         -f file   - use file instead of stdin/stdout\n"
         ++ "         -s steps  - number of steps to simulate when writing\n"
         ++ "         -h        - display help\n"


-------------------------------------------------------------------------------
------------------------------ Configuration ----------------------------------

-- |Simulation types that the program can be started up in
data SimulationTypes =
           Live             -- Live and interactive simulation
         | WriteToFile      -- No visualization. Write data to file
         | ReadFromFile     -- Read from file and play simulation
         | Help             -- Output help information
  deriving (Show, Read, Eq)

-- |Completly describes the configuration to run the simulation with
data Config =
 Config { birdCount :: Int              -- Number of birds in the simulation
        , simType :: SimulationTypes    -- Simulation type
        , simulationSteps :: Int        -- Number of steps to run when writing
        , file :: Maybe String          -- File to use when reading and writing
        }
  deriving (Show, Read, Eq)

-- |Default configuration for the program run without any commandline arguments
defaultConfig = Config 4 Live 300 Nothing

-- |Parse commandline arguments into a config
parseArguments :: [String] -> Config
parseArguments [] = defaultConfig
parseArguments ("-n":n:xs) = (parseArguments xs) { birdCount = int n }
parseArguments ("-s":n:xs) = (parseArguments xs) { simulationSteps = int n }
parseArguments ("-l":xs) = (parseArguments xs) { simType = Live }
parseArguments ("-o":xs) = (parseArguments xs) { simType = WriteToFile }
parseArguments ("-i":xs) = (parseArguments xs) { simType = ReadFromFile }
parseArguments ("-f":f:xs) = (parseArguments xs) { file = Just f }
parseArguments ("-h":xs) = (parseArguments xs) { simType = Help }
parseArguments xs = error $ "Unrecognized commandline arguments: " ++ show xs

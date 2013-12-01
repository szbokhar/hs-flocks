module Main where

import Control.Applicative                      ( (<$>) )
import Control.Monad                            ( foldM_  )
import Data.Maybe                               ( isNothing, fromJust )
import Graphics.Gloss.Interface.IO.Game         ( playIO )
import Graphics.Gloss.Interface.Pure.Simulate   ( simulate, white
                                                , Display( InWindow ) )
import System.Environment                       ( getArgs )

import qualified System.IO as F

import Animal       ( makeFlock )
import Water        ( makeFluid )
import Simulate     ( Drawable(..), Simulate(..), Sim(..))
import Utilities    ( int, (|>) )

-- |Main function
main :: IO ()
main = do
    -- Parse Arguments
    config <- getArgs |> parseArguments
    -- Initialize simulation
    initFlock <- simChoose config (simIndex config)

    -- Execute run mode
    case simType config of
        Live            -> runLiveSimulation initFlock
        WriteToFile     -> writeSimulationToFile config initFlock
        ReadFromFile    -> readSimulation config initFlock
        Help            -> showHelpMessage
  where
    -- Gloss winodw mode
    mode = InWindow "Boids" (1024,720) (20,20)

    -- Live simulation mode
    runLiveSimulation initFlock =
        playIO mode white 30 initFlock
            (return . draw)
            (\a b -> return $ react a b)
            updateIO

    -- Write simulation mode
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

    -- Render simulation form file or stdin
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

    -- Show help message
    showHelpMessage = putStrLn $
            "FlockSimulation\n"
         ++ "Options: -n count  - number of elements (default 4)\n"
         ++ "         -l        - run live simulation (default)\n"
         ++ "         -o        - write simulation to stdout\n"
         ++ "         -i        - display simulation read in from stdin\n"
         ++ "         -f file   - use file instead of stdin/stdout\n"
         ++ "         -s steps  - number of steps to simulate when writing\n"
         ++ "         -j [0,1]  - 0 for water sim, 1 for bird sim\n"
         ++ "         -h        - display help\n"

    -- Sets up the simulation based on the input integer
    simChoose :: Config -> Int -> IO Sim
    simChoose config 0 = Sim <$> makeFluid (640,480) (4,100) 1 (count config)
                           0.0          -- Target
                           0.1          -- Neighbour
                           5            -- Crowd
                           0.03         -- Damp
                           2          -- Gravity
                           (20, 40)     -- Neighbours
    simChoose config 1 = Sim <$> makeFlock (1024,720) (4,9) 1 (count config)
                           0.0          -- Target
                           4          -- Neighbour
                           5            -- Crowd
    simChoose _ _ = error "Unknown Simulation index"

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
 Config { count :: Int                  -- Number of birds in the simulation
        , simType :: SimulationTypes    -- Simulation type
        , simulationSteps :: Int        -- Number of steps to run when writing
        , file :: Maybe String          -- File to use when reading and writing
        , simIndex :: Int               -- Whether to run a water or bird sim
        }
  deriving (Show, Read, Eq)

-- |Default configuration for the program run without any commandline arguments
defaultConfig :: Config
defaultConfig = Config 4 Live 300 Nothing 0

-- |Parse commandline arguments into a config
parseArguments :: [String] -> Config
parseArguments [] = defaultConfig
parseArguments ("-n":n:xs) = (parseArguments xs) { count = int n }
parseArguments ("-s":n:xs) = (parseArguments xs) { simulationSteps = int n }
parseArguments ("-l":xs) = (parseArguments xs) { simType = Live }
parseArguments ("-o":xs) = (parseArguments xs) { simType = WriteToFile }
parseArguments ("-i":xs) = (parseArguments xs) { simType = ReadFromFile }
parseArguments ("-f":f:xs) = (parseArguments xs) { file = Just f }
parseArguments ("-h":xs) = (parseArguments xs) { simType = Help }
parseArguments ("-j":n:xs) = (parseArguments xs) { simIndex = int n }
parseArguments xs = error $ "Unrecognized commandline arguments: " ++ show xs

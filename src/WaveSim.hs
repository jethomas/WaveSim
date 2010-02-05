module WaveSim
   (main) where 

import Graphics.UI.GLUT
import Control.Exception
import System.Exit
import Data.IORef

import Graphics
import Config
import World
import Menu

main :: ConfigData -> IO ()
main cfg = do

   Graphics.initWindow winSize "Wave Simulator"
   Graphics.initGraphics (winWidth cfg) (winHeight cfg)

   worldState <- worldInit
   worldStateRef <- newIORef worldState

   -- GLUT callbacks for input
   --keyboardMouseCallback $= Just (worldInput (keysStateRef worldState))
   --motionCallback $= Just (worldMotion (mousePosRef worldState))
   --passiveMotionCallback $= Just (worldMotion (mousePosRef worldState))
   enterMainMenu worldStateRef drawMainMenu

   mainLoop

exitMain :: IO ()
exitMain = do
   throwIO $ ExitSuccess

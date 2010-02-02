module Main
   () where 

import Graphics.UI.GLUT
import Control.Exception
import System.Exit
--import Data.IORef

import Graphics
import DisplaySettings

main :: IO ()
main = do
   Graphics.initWindow winSize "Wave Simulator"
   Graphics.initGraphics winWidth winHeight

   --worldState <- worldInit
   --worldStateRef <- newIORef worldState

   --addTimerCallback 1 (programMain worldStateRef)

   mainLoop

exitMain :: IO ()
exitMain = do
   throwIO $ ExitSuccess

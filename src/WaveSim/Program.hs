module WaveSim.Program
   (mainDrawCallback,
    mainCallback) where

import Graphics.UI.GLUT
import Data.IORef
import WaveSim.Graphics
import WaveSim.Types
import WaveSim.Menu
import WaveSim.TwoD

mainDrawCallback :: IORef WorldState -> IO ()
mainDrawCallback worldStateRef = do
   worldState <- readIORef worldStateRef

   beginDraw
   case programState worldState of
      MainMenuState -> drawMainMenu worldStateRef
      TwoDWaveState -> drawTwoD worldStateRef
      ThreeDWaveState -> print "Not implemented..."
   endDraw


mainCallback :: IORef WorldState -> IO ()
mainCallback worldStateRef = do
   worldState <- readIORef worldStateRef

   case programState worldState of
      MainMenuState -> runMainMenu worldStateRef
      TwoDWaveState  -> runTwoD worldStateRef
      ThreeDWaveState -> print "Not implemented..."

   addTimerCallback (refreshRate (configData worldState)) (mainCallback worldStateRef)

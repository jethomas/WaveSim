module WaveSim.Program
   (mainDrawCallback,
    mainCallback) where

import Graphics.UI.GLUT
import Data.IORef
import WaveSim.Graphics
import WaveSim.Types
import WaveSim.Menu

mainDrawCallback :: IORef WorldState -> IO ()
mainDrawCallback worldStateRef = do
   worldState <- readIORef worldStateRef

   beginDraw
   case programState worldState of
      MainMenuState -> drawMainMenu worldStateRef
      TwoDWaveState -> print "Not implemented..."
      ThreeDWaveState -> print "Not implemented..."
   endDraw


mainCallback :: IORef WorldState -> IO ()
mainCallback worldStateRef = do
   worldState <- readIORef worldStateRef

   -- DEBUG
   case programState worldState of
      MainMenuState -> enterMainMenu worldStateRef
      TwoDWaveState  -> print "Not implemented..."
      ThreeDWaveState -> print "Not implemented..."

   addTimerCallback (refreshRate (configData worldState)) (mainCallback worldStateRef)

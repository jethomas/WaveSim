module WaveSim.Widgets
   (newButton) where

import Control.Monad
import Data.Maybe
import Data.IORef
import WaveSim.Types

onButton :: Int -> Int -> Button -> Bool
onButton x y but = x >= bx && x <= (bx + bw) && y >= by && y <= (by + bh)
   where bx = truncate $ xPosPoint $ ulRectPoint $ butGeometry $ but
         by = truncate $ yPosPoint $ ulRectPoint $ butGeometry $ but
         bw = truncate $ width $ butGeometry $ but
         bh = truncate $ height $ butGeometry $ but

activateButton :: MouseInfo -> Button -> Bool
activateButton mouseInfo' button =
   leftMouseDown mouseInfo' == False &&
   onButton (mouseX mouseInfo') (mouseY mouseInfo') button &&
   onButton (prevMouseXDown mouseInfo') (prevMouseYDown mouseInfo') button

buttonMouseCallback :: IORef WorldState -> Button -> IO ()
buttonMouseCallback worldStateRef button = do
   worldState <- readIORef worldStateRef
   when (activateButton (mouseInfo (inputStateRef worldState)) button) $
      (fromJust (butClickCall button)) worldStateRef

newButton :: IORef WorldState -> Button -> IO ()
newButton worldStateRef but = do
   worldState <- readIORef worldStateRef
   let mh = (buttonMouseCallback worldStateRef but):(mouseHandlers worldState)
   writeIORef worldStateRef (worldState {mouseHandlers = mh})

module WaveSim.Input
   (worldInput,
    worldMotion) where

import Graphics.UI.GLUT
import Data.IORef
import WaveSim.Types

worldInput :: IORef WorldState -> Key -> KeyState -> Modifiers -> Position -> IO()
worldInput worldStateRef key state _ pos = do
   worldState <- readIORef worldStateRef
   let inputState = inputStateRef worldState
   let wh = truncate $ winHeight (configData worldState)
   writeIORef worldStateRef (worldState {inputStateRef = keyAct key state pos wh inputState})
   sequence_ (mouseHandlers worldState)

worldMotion :: IORef WorldState -> Position -> IO ()
worldMotion worldStateRef (Position posX posY) = do
   worldState <- readIORef worldStateRef
   let inputState = inputStateRef worldState

   let x = fromIntegral posX
   let y = truncate $ winHeight (configData worldState) - fromIntegral posY
   let inputState' = inputState {mouseInfo = (mouseInfo inputState) {mouseX = x, mouseY = y}}

   writeIORef worldStateRef (worldState {inputStateRef = inputState'})
   -- Call mouse move callbacks here

keyAct :: Key -> KeyState -> Position -> Int -> InputState -> InputState
keyAct (MouseButton LeftButton) Down (Position posX posY) wh inputState =
   inputState {mouseInfo = mouseInfo'}
      where mouseInfo' = (mouseInfo inputState)
                              {
                                 leftMouseDown = True,
                                 prevMouseXDown = fromIntegral posX,
                                 prevMouseYDown = wh - fromIntegral posY
                              }

keyAct (MouseButton LeftButton) Up _ _ inputState =
   inputState {mouseInfo =  mouseInfo'}
      where mouseInfo' = (mouseInfo inputState) {leftMouseDown = False}

{-
keyAct (MouseButton RightButton) Down inputState =
   inputState {mouseInfo = mouseInfo'}
      where mouseInfo' = (mouseInfo inputState) {rightMouseDown = True}

keyAct (MouseButton RightButton) Up inputState =
   inputState {mouseInfo = mouseInfo'}
      where mouseInfo' = (mouseInfo inputState) {rightMouseDown = False}
-}

keyAct _ _ _ _ inputState = inputState


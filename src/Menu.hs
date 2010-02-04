module Menu
   (enterMainMenu,
    drawMainMenu) where

import Graphics.UI.GLUT as GLUT
import Control.Monad.State
import Data.IORef
import Data.Maybe
import DisplaySettings
import Types
import Graphics

twoDButtonGeometry :: (GLdouble, GLdouble, GLdouble, GLdouble)
twoDButtonGeometry = (x, y, w, h) where
                              x = 25
                              y = winHeight - 25 - h
                              w = 200
                              h = 55

twoDTextLocation :: (GLfloat, GLfloat)
twoDTextLocation = (58, (realToFrac winHeight) - 25 - 30)

threeDButtonGeometry :: (GLdouble, GLdouble, GLdouble, GLdouble)
threeDButtonGeometry = (x, y, w, h) where
                              x = 25
                              y = y' - 25
                              w = 200
                              h = 55
                              t = twoDButtonGeometry
                              y' = (\(_, y'', _, h') -> y'' - h') t

threeDTextLocation :: (GLfloat, GLfloat)
threeDTextLocation = (58, (realToFrac winHeight) - 25 - 55 - 25 - 30)

backGeometry :: (GLdouble, GLdouble, GLdouble, GLdouble)
backGeometry = (0, 0, winWidth, winHeight)

initMainMenu :: IO (Maybe MenuState)
initMainMenu = do
{-
   backTexture' <- liftIO $ loadTexture "data/menu_back.png"
   twoDButton' <- liftIO $ loadTexture "data/2Dbutton.png"
   threeDButton' <- liftIO $ loadTexture "data/3Dbutton.png"
   return (Just $ MenuState backTexture' twoDButton' threeDButton')
-}
   return Nothing

enterMainMenu :: IORef WorldState -> (IORef WorldState -> IO()) -> IO ()
enterMainMenu worldStateRef mainCallback = do
   worldState <- readIORef worldStateRef
   menuState' <- if isNothing (menuState worldState) == True
                  then initMainMenu
                  else return (menuState worldState)

   --- Update current ref
   let worldState' = WorldState MainMenuState menuState'
   writeIORef worldStateRef worldState'

   -- XXX DEBUG
   addTimerCallback 50 (mainCallback worldStateRef)

drawMainMenu :: IORef WorldState -> IO ()
drawMainMenu worldStateRef = do
   worldState <- readIORef worldStateRef

   beginDraw
   drawRect backGeometry (Color4 0 0 0 1)
   drawRect twoDButtonGeometry (Color4 1 1 1 1)
   drawRect threeDButtonGeometry (Color4 1 1 1 1)
   drawString twoDTextLocation "2-Dimensional Display" (Color4 0 0 0 1)
   drawString threeDTextLocation "3-Dimensional Display" (Color4 0 0 0 1)
   endDraw

   -- XXX DEBUG
   addTimerCallback 50 (drawMainMenu worldStateRef)


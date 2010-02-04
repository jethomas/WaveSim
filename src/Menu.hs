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

initMainMenu :: IO (Maybe MenuState)
initMainMenu = do
   backTexture' <- liftIO $ loadTexture "data/menu_back.png"
   twoDButton' <- liftIO $ loadTexture "data/2Dbutton.png"
   threeDButton' <- liftIO $ loadTexture "data/3Dbutton.png"
   return (Just $ MenuState backTexture' twoDButton' threeDButton')

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
   addTimerCallback 1000 (mainCallback worldStateRef)

drawMainMenu :: IORef WorldState -> IO ()
drawMainMenu worldStateRef = do
   worldState <- readIORef worldStateRef
   let menuState' = fromJust $ menuState worldState
   beginDraw
   drawTexture 0 0 winWidth winHeight (backTexture menuState') 1.0
   drawTexture 10 10 100 25 (twoDButton menuState') 1.0
   drawTexture 10 45 100 25 (threeDButton menuState') 1.0
   endDraw


module WaveSim.Menu
   (enterMainMenu,
    drawMainMenu) where

import Graphics.UI.GLUT as GLUT
import Control.Monad.State
import Data.IORef
import Data.Maybe
import Paths_WaveSim
import WaveSim.Graphics
import WaveSim.Types

initMainMenu :: IO (Maybe MenuState)
initMainMenu = do
   backTexture' <- liftIO $ loadTexture $ getDataFileName "data/menu_back.png"
   twoDButton' <- liftIO $ loadTexture $ getDataFileName "data/2Dbutton.png"
   threeDButton' <- liftIO $ loadTexture $ getDataFileName "data/3Dbutton.png"
   return (Just $ MenuState backTexture' twoDButton' threeDButton')

enterMainMenu :: IORef WorldState -> (IORef WorldState -> IO()) -> IO ()
enterMainMenu worldStateRef mainCallback = do
   worldState <- readIORef worldStateRef
   menuState' <- if isNothing (menuState worldState) == True
                  then initMainMenu
                  else return (menuState worldState)

   --- Update current ref
   let worldState' = WorldState (configData worldState) MainMenuState menuState'
   writeIORef worldStateRef worldState'

drawMainMenu :: IORef WorldState -> IO ()
drawMainMenu worldStateRef = do
   worldState <- readIORef worldStateRef

   -- Clear the old screen
   beginDraw

   -- Extra from world state
   let cfgData = configData worldState
   let menuData = fromJust $ menuState worldState

   -- Locations of visible objects
   let backGeometry = (0, 0, winWidth cfgData, winHeight cfgData)
   let twoDButtonGeometry = (25, (winHeight cfgData) - 80, 200, 55)
   let threeDButtonGeometry = (25, (winHeight cfgData) - 160, 200, 55)
   let twoDTextLocation = (58, (realToFrac (winHeight cfgData)) - 55)
   let threeDTextLocation = (58, (realToFrac (winHeight cfgData)) - 135)

   --drawRect backGeometry (Color4 0 0 0 1)
   --drawRect twoDButtonGeometry (Color4 1 1 1 1)
   --drawRect threeDButtonGeometry (Color4 1 1 1 1)
   drawTexture backGeometry (backTexture menuData) 1.0
   drawTexture twoDButtonGeometry (twoDButton menuData) 1.0
   drawTexture threeDButtonGeometry (threeDButton menuData) 1.0
   drawString twoDTextLocation "2-Dimensional Display" (Color4 0 0 0 1) (fontName cfgData)
   drawString threeDTextLocation "3-Dimensional Display" (Color4 0 0 0 1) (fontName cfgData)

   -- Display to screen
   endDraw


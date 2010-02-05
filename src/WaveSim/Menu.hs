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

initMainMenu :: Config -> IO (Config)
initMainMenu cfg = do

   let mainMenu' = mainMenu cfg
   let twoDButton' = twoDButton mainMenu'
   let threeDButton' = threeDButton mainMenu'
   let background' = background mainMenu'

   -- Missing butClickTex, for now
   twoDButtonTex <- if isNothing (butTex twoDButton') == True
                        then liftIO $ loadTexture $ getDataFileName "data/2Dbutton.png"
                        else return $ fromJust $ butTex twoDButton'
   threeDButtonTex <- if isNothing (butTex threeDButton') == True
                        then liftIO $ loadTexture $ getDataFileName "data/3Dbutton.png"
                        else return $ fromJust $ butTex threeDButton'
   backTexture' <- if isNothing (backTex background') == True
                        then liftIO $ loadTexture $ getDataFileName "data/menu_back.png"
                        else return $ fromJust $ backTex background'
   return $ cfg
      {
         mainMenu = mainMenu'
            {
               twoDButton = twoDButton' { butTex = Just twoDButtonTex },
               threeDButton = threeDButton' { butTex = Just threeDButtonTex },
               background = background' { backTex = Just backTexture' }
            }
      }

enterMainMenu :: IORef WorldState -> IO ()
enterMainMenu worldStateRef = do
   worldState <- readIORef worldStateRef
   configData' <- initMainMenu (configData worldState)

   --- Update current ref
   let worldState' = WorldState configData' MainMenuState
   writeIORef worldStateRef worldState'

drawMainMenu :: IORef WorldState -> IO ()
drawMainMenu worldStateRef = do
   worldState <- readIORef worldStateRef

   -- Extract from world state
   let cfgData = configData worldState
   let menuData = mainMenu cfgData

   -- References to the texture
   let twoDTex = fromJust $ butTex (twoDButton menuData)
   let threeDTex = fromJust $ butTex (threeDButton menuData)
   let menuTex = fromJust $ backTex (background menuData)

   drawTexture (backGeometry (background menuData)) menuTex 1.0
   drawTexture (butGeometry (twoDButton menuData)) twoDTex 1.0
   drawTexture (butGeometry (threeDButton menuData)) threeDTex 1.0
   drawString (twoDTextLoc menuData) "2-Dimensional Display" (Color4 0 0 0 1) (fontName cfgData)
   drawString (threeDTextLoc menuData) "3-Dimensional Display" (Color4 0 0 0 1) (fontName cfgData)


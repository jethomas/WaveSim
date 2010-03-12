module WaveSim.Menu
   (runMainMenu,
    drawMainMenu) where

import Graphics.UI.GLUT as GLUT
import Control.Monad.State
import Data.IORef
import Data.Maybe
import Paths_WaveSim
import WaveSim.Graphics
import WaveSim.Widgets
import WaveSim.Types
import WaveSim.TwoD

-- DEBUG
enterThreeD :: IORef WorldState -> IO ()
enterThreeD worldStateRef = putStrLn "Entering 3d."
-- END DEBUG

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
               menuInitComplete = True,
               twoDButton = twoDButton'
                  {
                     butTex = Just twoDButtonTex,
                     butClickCall = Just runTwoD
                  },
               threeDButton = threeDButton'
                  {
                     butTex = Just threeDButtonTex,
                     butClickCall = Just enterThreeD
                  },
               background = background' { backTex = Just backTexture' }
            }
      }

runMainMenu :: IORef WorldState -> IO ()
runMainMenu worldStateRef = do
   worldState <- readIORef worldStateRef

   -- Only initialize the main menu if we are displaying for the first time
   let initMenu = do newcfg <- initMainMenu (configData worldState)
                     writeIORef worldStateRef (worldState {configData = newcfg})
                     newButton worldStateRef (twoDButton (mainMenu newcfg))
                     newButton worldStateRef (threeDButton (mainMenu newcfg))

   unless (menuInitComplete (mainMenu (configData worldState))) initMenu

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


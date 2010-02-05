module WaveSim.WaveSim
   (waveSim,
    defaultConfig) where

import qualified Config.Dyre as Dyre
import Graphics.UI.GLUT
import Data.IORef

import WaveSim.Graphics
import WaveSim.Types
import WaveSim.Menu

defaultConfig :: Config
defaultConfig = Config
   {
      fontName          = Helvetica12,
      winHeight         = 600.0,
      winWidth          = 800.0,
      winSize           = Size (truncate (winWidth defaultConfig)) (truncate (winHeight defaultConfig)),
      refreshRate       = 16,
      errorMsg          = Nothing
   }

confError :: Config -> String -> Config
confError cfg errs = cfg {errorMsg = Just $ "Error:" ++ errs ++ "\n"}

waveSim :: Config -> IO ()
waveSim = Dyre.wrapMain $ Dyre.defaultParams
   {
      Dyre.projectName  = "WaveSim",
      Dyre.showError    = confError,
      Dyre.realMain     = realMain
   }

realMain :: Config -> IO ()
realMain cfg = do
   initWindow (winSize cfg) "Wave Simulator"
   initGraphics (winWidth cfg) (winHeight cfg)

   let worldState = WorldState cfg MainMenuState Nothing
   worldStateRef <- newIORef worldState

   displayCallback $= (drawMainMenu worldStateRef)

   -- DEBUG
   enterMainMenu worldStateRef drawMainMenu

   mainLoop


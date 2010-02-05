module WaveSim.WaveSim
   (waveSim,
    defaultConfig) where

import qualified Config.Dyre as Dyre
import Graphics.UI.GLUT
import Data.IORef

import WaveSim.Graphics
import WaveSim.Program
import WaveSim.Types

defaultConfig :: Config
defaultConfig = Config
   {
      fontName             = Helvetica12,
      winHeight            = 600.0,
      winWidth             = 800.0,
      winSize              = Size (truncate (winWidth defaultConfig)) (truncate (winHeight defaultConfig)),
      refreshRate          = 16,
      errorMsg             = Nothing,
      mainMenu             = MainMenu
         {
            twoDButton     = Button
               {
                  butGeometry    = WRect { ulRectPoint = WPoint 25.0 520.0, width = 200, height = 55 },
                  butTex         = Nothing,
                  butClickTex    = Nothing
               },
            threeDButton   = Button
               {
                  butGeometry    = WRect { ulRectPoint = WPoint 25.0 440.0, width = 200, height = 55 },
                  butTex         = Nothing,
                  butClickTex    = Nothing
               },
            background     = Background
               {
                  backGeometry   = WRect { ulRectPoint = WPoint 0.0 0.0, width = 800, height = 600 },
                  backTex        = Nothing
               },
            twoDTextLoc    = WPointFloat { xPosPointFloat = 50, yPosPointFloat = 543 },
            threeDTextLoc  = WPointFloat { xPosPointFloat = 50, yPosPointFloat = 465 }
         }
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

   let worldState = WorldState cfg MainMenuState
   worldStateRef <- newIORef worldState

   mainCallback worldStateRef
   displayCallback $= (mainDrawCallback worldStateRef)

   mainLoop


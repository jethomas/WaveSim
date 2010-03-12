module WaveSim.WaveSim
   (waveSim,
    defaultConfig) where

import qualified Config.Dyre as Dyre
import Graphics.UI.GLUT
import Data.IORef

import WaveSim.Graphics
import WaveSim.Program
import WaveSim.Input
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
            menuInitComplete = False,
            twoDButton     = Button
               {
                  butGeometry    = WRect { ulRectPoint = WPoint 25.0 520.0, width = 200, height = 55 },
                  butTex         = Nothing,
                  butClickTex    = Nothing,
                  butClickCall   = Nothing
               },
            threeDButton   = Button
               {
                  butGeometry    = WRect { ulRectPoint = WPoint 25.0 440.0, width = 200, height = 55 },
                  butTex         = Nothing,
                  butClickTex    = Nothing,
                  butClickCall   = Nothing
               },
            background     = Background
               {
                  backGeometry   = WRect { ulRectPoint = WPoint 0.0 0.0, width = 800, height = 600 },
                  backTex        = Nothing
               },
            twoDTextLoc    = WPointFloat { xPosPointFloat = 50, yPosPointFloat = 543 },
            threeDTextLoc  = WPointFloat { xPosPointFloat = 50, yPosPointFloat = 465 }
         },
      twoD                 = TwoDWave
         {
            twoDInitComplete = False,
            amplitude = 100,
            omega = pi / 2.0,
            particlesToShow = 16.0,
            cyclesToShow = 2.0,
            particleTex = Nothing,
            time = 0,
            lastTime = undefined,
            function = Just sin
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

   let inputState = InputState (MouseInfo 0 0 False 0 0)
   worldStateRef <- newIORef (WorldState cfg MainMenuState [] inputState)

   keyboardMouseCallback $= Just (worldInput worldStateRef)
   motionCallback $= Just (worldMotion worldStateRef)
   passiveMotionCallback $= Just (worldMotion worldStateRef)

   mainCallback worldStateRef
   displayCallback $= (mainDrawCallback worldStateRef)

   mainLoop


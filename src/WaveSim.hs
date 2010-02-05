module WaveSim
   (waveSim,
    ConfigData(ConfigData),
    fontName,
    winHeight,
    winWidth,
    winSize,
    refreshRate,
    defaultConfig) where

import qualified Config.Dyre as Dyre
import Graphics.UI.GLUT
import Data.IORef

import Graphics
import World
import Menu

data ConfigData = ConfigData
   {
      fontName    :: BitmapFont,
      winHeight   :: GLdouble,
      winWidth    :: GLdouble,
      winSize     :: Size,
      refreshRate :: Int
   }

defaultConfig = ConfigData
   {
      fontName       = Helvetica12,
      winHeight      = 600.0,
      winWidth       = 800.0,
      winSize        = Size (truncate (winWidth defaultConfig)) (truncate (winHeight defaultConfig)),
      refreshRate    = 16
   }

confError cfgMessage error = "Error:" ++ error ++ "\n" ++ cfgMessage

waveSim = Dyre.wrapMain Dyre.defaultParams
   {
      Dyre.projectName   = "WaveSim",
      Dyre.showError     = confError,
      Dyre.realMain      = main
   }

main :: ConfigData -> IO ()
main cfg = do
   Graphics.initWindow (winSize cfg) "Wave Simulator"
   Graphics.initGraphics (winWidth cfg) (winHeight cfg)

   worldState <- worldInit
   worldStateRef <- newIORef worldState

   -- DEBUG
   enterMainMenu worldStateRef drawMainMenu

   mainLoop


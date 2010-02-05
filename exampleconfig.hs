import WaveSim.WaveSim
import WaveSim.Types
import Graphics.UI.GLUT

w = 1000
h = 50

main :: IO ()
main = waveSim $ defaultConfig
   {
      winHeight = w,
      winWidth = h,
      winSize = Size (truncate w) (truncate h)
   }

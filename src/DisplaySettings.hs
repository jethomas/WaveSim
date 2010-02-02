module DisplaySettings
   (winSize,
    winHeight,
    winWidth,
    refreshRate) where

import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL

-- Export in a format GLUT likes
winSize :: Size
winSize = Size (truncate winHeight) (truncate winWidth)

-- Height of the main window
winHeight :: GLdouble
winHeight = 600.0

-- Width of the main window
winWidth :: GLdouble
winWidth = 800.0

-- In milliseconds
refreshRate :: Int
refreshRate = 16

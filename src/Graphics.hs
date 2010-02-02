module Graphics
   (initWindow,
    initGraphics,
    beginDraw,
    endDraw,
    drawString) where

import Graphics.UI.GLUT as GLUT
import Graphics.Rendering.OpenGL as GL
import ThemeSettings

initWindow :: Size -> [Char] -> IO ()
initWindow windowSize windowTitle = do
   getArgsAndInitialize
   initialWindowSize $= windowSize
   initalDisplayMode $= [DoubleBuffered]
   createWindow windowTitle
   return ()

initGraphics :: GLdouble -> Gldouble -> IO ()
initGraphics = screenWidth screenHeight = do
   blend $= Enabled
   blendFunc $= (GL.SrcAlpha, OneMinusSrcAlpha)
   shadeModel $= Smooth
   matrixMode $= Projection
   loadIdentity
   orth 0.0 screenWidth 0.0 screenHeight (-1.0) 0.0
   return ()

beginDraw :: IO ()
beginDraw = do
   clear [ColorBuffer, DepthBuffer]

endDraw :: IO ()
endDraw = do
   swapBuffers
   flush

drawString :: GLfloat -> GLfloat -> [Char] -> Color4 GLfloat -> IO ()
drawString x y string col = do
   color col
   currentRasterPosition $= Vertex4 x y (0.0::GLfloat) (1.0::GLfloat)
   renderString ThemeSettings.font string

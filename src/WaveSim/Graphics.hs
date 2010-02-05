module WaveSim.Graphics
   (initWindow,
    initGraphics,
    beginDraw,
    endDraw,
    drawString,
    loadTexture,
    drawTexture,
    drawRect) where

import Graphics.UI.GLUT as GLUT
import Graphics.Rendering.OpenGL as GL
import Graphics.UI.SDL.Image as SDLImage
import Graphics.UI.SDL.Video
import Graphics.UI.SDL.Types
import Control.Monad
import WaveSim.Types

initWindow :: Size -> [Char] -> IO ()
initWindow wSize wTitle = do
   _ <- getArgsAndInitialize
   initialWindowSize $= wSize
   initialDisplayMode $= [DoubleBuffered]
   _ <- createWindow wTitle
   return ()

initGraphics :: GLdouble -> GLdouble -> IO ()
initGraphics screenWidth screenHeight = do
   blend $= Enabled
   blendFunc $= (GL.SrcAlpha, OneMinusSrcAlpha)
   shadeModel $= Smooth
   matrixMode $= Projection
   loadIdentity
   ortho 0.0 screenWidth 0.0 screenHeight (-1.0) 0.0
   return ()

beginDraw :: IO ()
beginDraw = do
   clear [ColorBuffer, DepthBuffer]

endDraw :: IO ()
endDraw = do
   swapBuffers
   flush

drawString :: WPointFloat -> [Char] -> Color4 GLfloat -> BitmapFont -> IO ()
drawString pos string col font = do
   color col
   let x = xPosPointFloat pos
   let y = yPosPointFloat pos
   currentRasterPosition $= Vertex4 x y (0.0::GLfloat) (1.0::GLfloat)
   renderString font string

loadTexture :: IO (FilePath) -> IO (WTexture)
loadTexture fpath = do
   -- Use SDL to load the image
   path <- fpath
   surface <- SDLImage.loadTyped path SDLImage.PNG

   let w = fromIntegral (surfaceGetWidth surface)
   let h = fromIntegral (surfaceGetHeight surface)
   let size = TextureSize2D w h

   -- Transfer data to OpenGL
   textureObj <- liftM head (genObjectNames 1)
   textureBinding Texture2D $= Just textureObj
   textureWrapMode Texture2D S $= (Repeated, Repeat)
   textureWrapMode Texture2D T $= (Repeated, Repeat)
   textureFilter Texture2D $= ((Linear', Nothing), Linear')
   surfacePixels <- surfaceGetPixels surface
   bytesPerPixel <- pixelFormatGetBytesPerPixel (surfaceGetPixelFormat surface)

   let pixelFormat = if bytesPerPixel == 3 then RGB else RGBA
   let pixelData = PixelData pixelFormat UnsignedByte surfacePixels

   texImage2D Nothing NoProxy 0 RGBA' size 0 pixelData

   -- Free the SDL surface
   freeSurface surface
   return (WTexture w h textureObj)

freeTexture :: WTexture -> IO ()
freeTexture tex = do
   deleteObjectNames ([textureObject tex])

drawTexture :: WRect -> WTexture -> GLdouble -> IO ()
drawTexture geo tex alpha = do
   texture Texture2D $= Enabled
   textureBinding Texture2D $= Just (textureObject tex)

   let x = xPosPoint (ulRectPoint geo)
   let y = yPosPoint (ulRectPoint geo)
   let w = width geo
   let h = height geo

   let width' = if w <= 0
                  then fromIntegral $ textureWidth tex
                  else w
   let height' = if h <= 0
                  then fromIntegral $ textureHeight tex
                  else h

   let texCoord2f = texCoord :: TexCoord2 GLdouble -> IO ()
       vertex3f = vertex :: Vertex3 GLdouble -> IO ()
       color4f = color :: Color4 GLdouble -> IO ()
       col = color4f (Color4 (1.0 :: GLdouble) (1.0 :: GLdouble) (1.0 :: GLdouble) alpha)

   renderPrimitive Quads $ do
      texCoord2f (TexCoord2 0 1); vertex3f (Vertex3 x y 0.0); col
      texCoord2f (TexCoord2 0 0); vertex3f (Vertex3 x (y + height') 0.0); col
      texCoord2f (TexCoord2 1 0); vertex3f (Vertex3 (x + width') (y + height') 0.0); col
      texCoord2f (TexCoord2 1 1); vertex3f (Vertex3 (x + width') y 0.0); col

   texture Texture2D $= Disabled

drawRect :: (GLdouble, GLdouble, GLdouble, GLdouble) -> Color4 GLdouble -> IO ()
drawRect (x, y, w, h) col = do
   let vertices = [Vertex3 x y 0.0,
                   Vertex3 (x + w) y 0.0,
                   Vertex3 (x + w) (y + h) 0.0,
                   Vertex3 x (y + h) 0.0]

   renderPrimitive Quads $ do
      mapM_ (\x' -> color x') [col]
      mapM_ (\x' -> vertex x') vertices


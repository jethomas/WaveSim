module WaveSim.TwoD
   (runTwoD,
    drawTwoD) where

import Graphics.UI.GLUT as GLUT
import Control.Monad.State
import Data.IORef
import Data.Maybe
import Data.Time.Clock
import Paths_WaveSim
import WaveSim.Graphics
import WaveSim.Types

initTwoD :: Config -> IO (Config)
initTwoD cfg = do

   let twoD' = twoD cfg
   let particleTex' = particleTex twoD'

   tex <- if isNothing (particleTex twoD') == True
            then liftIO $ loadTexture $ getDataFileName "data/2DParticle.png"
            else return $ fromJust $ particleTex'

   return $ cfg
      {
         twoD = twoD'
            {
               twoDInitComplete = True,
               particleTex = Just tex,
               time = 0,
               lastTime = getCurrentTime
            }
      }

runTwoD :: IORef WorldState -> IO ()
runTwoD worldStateRef = do
   worldState <- readIORef worldStateRef

   let initTwoD' = do newcfg <- initTwoD (configData worldState)
                      writeIORef worldStateRef (worldState {configData = newcfg, programState = TwoDWaveState})

   let resetTime = writeIORef worldStateRef (worldState {configData = resetcfg})
       twoD' = (twoD (configData worldState)) {time = 0, lastTime = getCurrentTime}
       resetcfg = (configData worldState) {twoD = twoD'}
       runTwoD' = if (twoDInitComplete (twoD (configData worldState)))
                     then resetTime
                     else initTwoD'

   unless (programState worldState == TwoDWaveState) runTwoD'

   -- Draw our updates
   beginDraw
   drawTwoD worldStateRef
   endDraw

drawTwoD :: IORef WorldState -> IO ()
drawTwoD worldStateRef = do
   worldState <- readIORef worldStateRef

   let cfgData = configData worldState
       twoDData = twoD cfgData
       tex = fromJust $ particleTex twoDData
       pixelsPerCycle = ((winWidth cfgData) - 100) / (realToFrac (cyclesToShow twoDData))
       pixelsPerParticle = (realToFrac pixelsPerCycle) / (particlesToShow twoDData)
       origin = (winHeight cfgData) / 2
       texCenterX = (fromIntegral (textureWidth tex)) / 2
       texCenterY = (fromIntegral (textureHeight tex)) / 2
       numParticles = floor ((particlesToShow twoDData) * (cyclesToShow twoDData))
       pixelsToRad = realToFrac (2 * pi / pixelsPerCycle)

   lastTime' <- lastTime twoDData
   currentTime <- getCurrentTime
   let time' = (time twoDData) + (realToFrac (1000 * (diffUTCTime lastTime' currentTime)))

   let generateGeometry x y = WRect (WPoint (x - texCenterX) (y - texCenterY + origin)) 0.0 0.0
       xList = map (+ (pixelsPerParticle / 2.0)) (take numParticles [0,pixelsPerParticle..])
       g x = (omega twoDData) * (time' / 1000) + (x * pixelsToRad)
       f x = ((fromJust (function twoDData)) (g x)) * (fromIntegral (amplitude twoDData))
       particleList = map (\x -> (x, f x)) xList

   let firstParticleX = (realToFrac (fst (head particleList))) + 50
       yPadding = 10
       thickness = 2
   -- Background and some rudimentary axis
   drawRect (0.0, 0.0, winWidth cfgData, winHeight cfgData) (Color4 1 1 1 1)
   drawRect (firstParticleX - texCenterX, ((winHeight cfgData) / 2) - (thickness / 2), winWidth cfgData, thickness) (Color4 1 0 0 1)
   drawRect (firstParticleX - (thickness / 2), yPadding, thickness, (winHeight cfgData) - (yPadding * 2)) (Color4 0 1 0 1)
   mapM_ (\(x, y) -> drawTexture (generateGeometry (realToFrac x + 50) (realToFrac y)) tex 1.0) particleList

   let cfgData' = cfgData {twoD = twoDData {lastTime = return (currentTime), time = time'}}
   writeIORef worldStateRef (worldState {configData = cfgData'})


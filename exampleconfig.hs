import WaveSim.WaveSim
import WaveSim.Types
import Graphics.UI.GLUT

period = 2.0 * pi
mod' x = x - (fromIntegral (floor (x / period))) * period
box x = if mod' x >= pi then 1.0 else 0.0
sawtooth x = 2 * (x / period - (fromIntegral (floor (x / period + 0.5))))
triangle x = 2 * (abs (sawtooth x) - 0.5)

twoD' = twoD defaultConfig

main :: IO ()
main = waveSim $ defaultConfig
   {
      twoD = twoD'
         {
            omega = pi / 8,
            amplitude = 100,
            cyclesToShow = 2,
            particlesToShow = 32,
            function = Just triangle
         }
   }


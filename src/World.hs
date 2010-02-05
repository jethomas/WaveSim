module World
   (worldInit) where

import WaveSim
import Types

worldInit :: ConfigData -> IO (WorldState)
worldInit cfg = do
   return (WorldState cfg MainMenuState Nothing)


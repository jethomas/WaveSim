module World
   (worldInit) where

import Types

worldInit :: IO (WorldState)
worldInit = do
   return (WorldState MainMenuState Nothing)


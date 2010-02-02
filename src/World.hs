module World
   (worldInit,
    ProgramState(MainMenuState,TwoDWaveState,ThreeDWaveState),
    WorldState(WorldState)) where

data ProgramState = MainMenuState | TwoDWaveState | ThreeDWaveState

data WorldState = WorldState
   {
      programState   :: ProgramState
   }

worldInit :: IO (WorldState)
worldInit = do
   return (WorldState MainMenuState)


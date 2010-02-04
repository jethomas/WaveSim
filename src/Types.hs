module Types
   (WTexture(WTexture),
    textureWidth,
    textureHeight,
    textureObject,
    ProgramState(MainMenuState,TwoDWaveState,ThreeDWaveState),
    WorldState(WorldState),
    programState,
    menuState,
    MenuState(MenuState),
    backTexture,
    twoDButton,
    threeDButton) where

import Graphics.Rendering.OpenGL

data WTexture = WTexture
   {
      textureWidth   :: GLsizei,
      textureHeight  :: GLsizei,
      textureObject  :: TextureObject
   }

data ProgramState = MainMenuState | TwoDWaveState | ThreeDWaveState

data WorldState = WorldState
   {
      programState   :: ProgramState,
      menuState      :: Maybe MenuState
   }

data MenuState = MenuState
   {
      backTexture    :: WTexture,
      twoDButton     :: WTexture,
      threeDButton   :: WTexture
   }

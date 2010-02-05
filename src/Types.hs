module Types
   (WTexture(WTexture),
    textureWidth,
    textureHeight,
    textureObject,
    WPoint(WPoint),
    xPosPoint,
    yPosPoint,
    WRect(WRect),
    ulRectPoint,
    urRectPoint,
    lrRectPoint,
    llRectPoint,
    ProgramState(MainMenuState,TwoDWaveState,ThreeDWaveState),
    WorldState(WorldState),
    configData,
    programState,
    menuState,
    MenuState(MenuState),
    backTexture,
    twoDButton,
    threeDButton) where

import Graphics.Rendering.OpenGL
import Config

data WTexture = WTexture
   {
      textureWidth   :: GLsizei,
      textureHeight  :: GLsizei,
      textureObject  :: TextureObject
   }

data WPoint = WPoint
   {
      xPosPoint      :: GLdouble,
      yPosPoint      :: GLdouble
   }

data WRect = WRect
   {
      ulRectPoint    :: WPoint,
      urRectPoint    :: WPoint,
      lrRectPoint    :: WPoint,
      llRectPoint    :: WPoint
   }

data ProgramState = MainMenuState | TwoDWaveState | ThreeDWaveState

data WorldState = WorldState
   {
      configData     :: ConfigData,
      programState   :: ProgramState,
      menuState      :: Maybe MenuState
   }

data MenuState = MenuState
   {
      backTexture    :: WTexture,
      twoDButton     :: WTexture,
      threeDButton   :: WTexture
   }

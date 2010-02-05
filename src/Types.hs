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
    MenuState(MenuState),
    backTexture,
    twoDButton,
    threeDButton,
    Config(Config),
    fontName,
    winHeight,
    winWidth,
    winSize,
    refreshRate,
    errorMsg,
    WorldState(WorldState),
    configData,
    programState,
    menuState,) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

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

data MenuState = MenuState
   {
      backTexture    :: WTexture,
      twoDButton     :: WTexture,
      threeDButton   :: WTexture
   }

data Config = Config
   {
      fontName          :: BitmapFont,
      winHeight         :: GLdouble,
      winWidth          :: GLdouble,
      winSize           :: Size,
      refreshRate       :: Int,
      errorMsg          :: Maybe String
   }

data WorldState = WorldState
   {
      configData        :: Config,
      programState      :: ProgramState,
      menuState         :: Maybe MenuState
   }


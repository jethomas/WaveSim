module WaveSim.Types
   (Button(Button),
    butGeometry,
    butTex,
    butClickTex,
    MainMenu(MainMenu),
    twoDButton,
    threeDButton,
    background,
    twoDTextLoc,
    threeDTextLoc,
    Background(Background),
    backGeometry,
    backTex,
    WTexture(WTexture),
    textureWidth,
    textureHeight,
    textureObject,
    WPoint(WPoint),
    xPosPoint,
    yPosPoint,
    WPointFloat(WPointFloat),
    xPosPointFloat,
    yPosPointFloat,
    WRect(WRect),
    ulRectPoint,
    width,
    height,
    ProgramState(MainMenuState,TwoDWaveState,ThreeDWaveState),
    Config(Config),
    fontName,
    winHeight,
    winWidth,
    winSize,
    refreshRate,
    errorMsg,
    mainMenu,
    WorldState(WorldState),
    configData,
    programState) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

data Button = Button
   {
      butGeometry    :: WRect,
      butTex         :: Maybe WTexture,
      butClickTex    :: Maybe WTexture
   }

data MainMenu = MainMenu
   {
      twoDButton     :: Button,
      threeDButton   :: Button,
      background     :: Background,
      twoDTextLoc    :: WPointFloat,
      threeDTextLoc  :: WPointFloat
   }

data Background = Background
   {
      backGeometry   :: WRect,
      backTex        :: Maybe WTexture
   }

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

data WPointFloat = WPointFloat
   {
      xPosPointFloat :: GLfloat,
      yPosPointFloat :: GLfloat
   }

data WRect = WRect
   {
      ulRectPoint    :: WPoint,
      width          :: GLdouble,
      height         :: GLdouble
   }

data ProgramState = MainMenuState | TwoDWaveState | ThreeDWaveState

data Config = Config
   {
      fontName          :: BitmapFont,
      winHeight         :: GLdouble,
      winWidth          :: GLdouble,
      winSize           :: Size,
      refreshRate       :: Int,
      errorMsg          :: Maybe String,
      mainMenu          :: MainMenu
   }

data WorldState = WorldState
   {
      configData        :: Config,
      programState      :: ProgramState
   }


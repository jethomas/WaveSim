module WaveSim.Types
   (Button(Button),
    butGeometry,
    butTex,
    butClickTex,
    butClickCall,
    MainMenu(MainMenu),
    menuInitComplete,
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
    twoD,
    WorldState(WorldState),
    configData,
    programState,
    mouseHandlers,
    inputStateRef,
    MouseInfo(MouseInfo),
    mouseX,
    mouseY,
    leftMouseDown,
    prevMouseXDown,
    prevMouseYDown,
    InputState(InputState),
    mouseInfo,
    TwoDWave(TwoDWave),
    twoDInitComplete,
    amplitude,
    omega,
    particlesToShow,
    cyclesToShow,
    particleTex,
    time,
    lastTime,
    function) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
import Data.Time.Clock

data Button = Button
   {
      butGeometry       :: WRect,
      butTex            :: Maybe WTexture,
      butClickTex       :: Maybe WTexture,
      butClickCall      :: Maybe (IORef WorldState -> IO ())
   }

data TwoDWave = TwoDWave
   {
      twoDInitComplete  :: Bool,
      amplitude         :: Int,        -- Just pixels
      omega             :: Double,     -- This is in rad/s
      particlesToShow   :: Double,     -- Per 2pi
      cyclesToShow      :: Double,     -- The number of complete wave cycles to show
      particleTex       :: Maybe WTexture,
      time              :: Double,
      lastTime          :: IO UTCTime,
      function          :: Maybe (Double -> Double)
   }

data MainMenu = MainMenu
   {
      menuInitComplete  :: Bool,
      twoDButton        :: Button,
      threeDButton      :: Button,
      background        :: Background,
      twoDTextLoc       :: WPointFloat,
      threeDTextLoc     :: WPointFloat
   }

data Background = Background
   {
      backGeometry      :: WRect,
      backTex           :: Maybe WTexture
   }

data WTexture = WTexture
   {
      textureWidth      :: GLsizei,
      textureHeight     :: GLsizei,
      textureObject     :: TextureObject
   }

data WPoint = WPoint
   {
      xPosPoint         :: GLdouble,
      yPosPoint         :: GLdouble
   }

data WPointFloat = WPointFloat
   {
      xPosPointFloat    :: GLfloat,
      yPosPointFloat    :: GLfloat
   }

data WRect = WRect
   {
      ulRectPoint       :: WPoint,
      width             :: GLdouble,
      height            :: GLdouble
   }

data ProgramState = MainMenuState | TwoDWaveState | ThreeDWaveState deriving (Eq)

data Config = Config
   {
      fontName          :: BitmapFont,
      winHeight         :: GLdouble,
      winWidth          :: GLdouble,
      winSize           :: Size,
      refreshRate       :: Int,
      errorMsg          :: Maybe String,
      mainMenu          :: MainMenu,
      twoD              :: TwoDWave
   }

data WorldState = WorldState
   {
      configData        :: Config,
      programState      :: ProgramState,
      mouseHandlers     :: [IO ()],
      inputStateRef     :: InputState
   }

data MouseInfo = MouseInfo
   {
      mouseX            :: Int,
      mouseY            :: Int,
      leftMouseDown     :: Bool,
      prevMouseXDown    :: Int,
      prevMouseYDown    :: Int
   }

data InputState = InputState
   {
      mouseInfo         :: MouseInfo
   }


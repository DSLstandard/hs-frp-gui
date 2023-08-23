module Vg.Sdl where

import           Control.Lens
import           Data.Geometry
import           FRP.BearRiver
import           Relude
import qualified SDL
import           Vg.Math

data SdlIn
  = SdlIn
      { _events            :: [SDL.Event]
      , _currentWindowSize :: Vector 2 Int
      }
  deriving (Show)
makeFieldsNoPrefix ''SdlIn

catchEvent :: Monad m => (SDL.EventPayload -> Event a) -> MSF m SdlIn (Event a)
catchEvent f = arr $ asumMap (f . SDL.eventPayload) . view events

windowSize :: Monad m => MSF m SdlIn (Vector 2 Int)
windowSize = arr (view currentWindowSize)

windowClose :: Monad m => MSF m SdlIn (Event ())
windowClose = catchEvent \ev -> do
  SDL.WindowClosedEvent SDL.WindowClosedEventData{..} <- pure ev
  pure ()

mousePosition :: Monad m => SF m SdlIn (Point 2 Int)
mousePosition = hold origin <<< catchEvent \ev -> do
  SDL.MouseMotionEvent SDL.MouseMotionEventData{..} <- pure ev
  pure $ fmap fromIntegral (mouseMotionEventPos^.hgeometry)

mousePressed :: Monad m => SDL.MouseButton -> SF m SdlIn (Event ())
mousePressed btn = catchEvent \ev -> do
  SDL.MouseButtonEvent SDL.MouseButtonEventData{..} <- pure ev
  guard $ mouseButtonEventButton == btn && mouseButtonEventMotion == SDL.Pressed

mouseReleased :: Monad m => SDL.MouseButton -> SF m SdlIn (Event ())
mouseReleased btn = catchEvent \ev -> do
  SDL.MouseButtonEvent SDL.MouseButtonEventData{..} <- pure ev
  guard $ mouseButtonEventButton == btn && mouseButtonEventMotion == SDL.Released

deriving instance Eq SDL.SystemCursor
deriving instance Show SDL.SystemCursor
deriving instance Ord SDL.SystemCursor
deriving instance Enum SDL.SystemCursor
deriving instance Bounded SDL.SystemCursor

data SdlOut
  = SdlOut
      { _render    :: IO ()
      , _quitReq   :: Event ()
      , _cursorReq :: Event SDL.Cursor
        -- ^ Set new active cursor
      }
makeFieldsNoPrefix ''SdlOut

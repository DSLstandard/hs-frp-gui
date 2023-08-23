module Main where

import           Control.Lens
import           Data.Colour
import           Data.Colour.Names
import           Data.Ext
import           Data.Geometry
import qualified Data.Geometry.Box as GB
import           Data.MonadicStreamFunction.InternalCore (MSF (unMSF))
import qualified Data.Set as Set
import           Data.StateVar (($=))
import qualified Data.StateVar as StateVar
import           FRP.BearRiver
import qualified Glew
import qualified Graphics.Rendering.OpenGL as GL
import qualified NanoVG as NVG
import           Relude
import qualified SDL
import           Vg.Frp
import           Vg.Math
import           Vg.Nvg
import           Vg.Sdl
import           Vg.Widget.Button
import           Vg.Widget.Scroll
import           Vg.Widget.Scroll (ScrollBar (_pageLength))
import           Vg.Widget.Select
import           Vg.Widget.Widget
import qualified Data.Vector as V
import qualified Data.Stream as S
import Vg.Widget.Field

data GuiOut
  = GuiOut
      { _sdlOut :: SdlOut
      , _action :: IO ()
      }
makeFieldsNoPrefix ''GuiOut

handler :: (MonadIO m) => NVG.Context -> NvgFont -> SF m SdlIn GuiOut
handler nvgctx nvgfont = proc (sdlin) -> do
  let guiin = GuiIn CropBox'Inf sdlin
  quit <- sdlin >- windowClose
  winsize <- sdlin >- windowSize

  mousepos <- sdlin >- mousePosition
  pressed  <- sdlin >- mousePressed SDL.ButtonLeft
  startpos <- pressed $> mousepos >- hold origin

  let font = VgFont nvgfont 14
  let btn1 = Button font "Hello World"
  let btn2 = Button font "NO WAY"
  let btn3 = Button font "course"

  let
    org = (Point2 0 0)
    lay = vPure (const (,))
      `vAp`
        ( ofMinSize (buttonMinSize btn1)
        )
      `vAp`
        ( hPure (const (,))
          `hAp` ofMinSize (buttonMinSize btn2)
          `hAp` ofMinSize (buttonMinSize btn3)
        )
    (btn1area, (btn2area, btn3area)) = layoutMinAt org lay
    fullarea = fromOrgSize org (lay^.minimumSize)

  (btn1w, btn1o) <- (guiin, btn1area, btn1) >- dynButton
  (btn2w, btn2o) <- (guiin, btn2area, btn2) >- dynButton
  (btn3w, btn3o) <- (guiin, btn3area, btn3) >- dynButton

  let
    bararea = fromOrgSize (Point2 100 100) (Vector2 commonScrollBarWidth 200)
    bar = ScrollBarCfg { _pageStep = 20, _pageLength = 100 }
  (barw, baro) <- (guiin, bararea, bar) >- dynScrollBarAuto

  let
    scararea = fromOrgSize (Point2 200 100) (Vector2 300 300)
  (scarw, cliento) <-
    (guiin, scararea, ScrollArea 1000, ()) >- dynScrollAreaAuto
      proc (guiin, clientarea, input) -> do
        let btnarea = takeBox yComponent 200 clientarea
        (guiin, btnarea, Button (VgFont nvgfont 13) "INSIDE") >- dynButton

  -- selitems <- () >- iterThru 1 $ S.cycle
  --   [ ["Hello", "world"]
  --   , ["Hello", "world", "12333333333333"]
  --   , ["Hello", "world", "12333333333333", "10257855555"]
  --   , ["Hello World", "Green Tea", "Funeral"]
  --   ]
  let
    selitems = ["HEllo", "WORLd", "123", "456", "MORE ITEMS", "item1", "item2", "item3"]
    sel = Select
      { _font = VgFont nvgfont 11
      , _choices = V.fromList selitems
      , _showMaxCount = Nothing
      }
    selarea = fromOrgSize (Point2 50 200) (selectMinSize sel)
  (selw, selo) <- (guiin, selarea, sel) >- dynSelect

  let
    field = Field (VgFont nvgfont 11) (Just "Enter here")
    fieldarea = fromOrgSize (Point2 300 50) (Vector2 100 (fieldMinHeight field))
  (fieldw, fieldo) <- (guiin, fieldarea, field) >- dynField

  let
    widget :: Widget = mconcat
      [ btn1w, btn2w, btn3w
      , barw, scarw, selw, fieldw
      , plainPicture ( stroke (CColor (opaque blue)) $ rectangle (borderize fullarea) )
      -- , Widget
      --     { _picture = mconcat
      --       [ fill (CColor (opaque yellowgreen))
      --           $ rectangle (areaize $ GB.box (ext startpos) (ext mousepos))
      --       , stroke (CColor (opaque blue))
      --           $ polygon (regularPolygon 5 (fmap fromIntegral (mousepos .-. startpos)) (fmap fromIntegral startpos))
      --       ]
      --     , _systemCursor = mempty
      --     }
      ]

  setcursor <- fromMaybe SDL.SystemCursorArrow (getLast (widget^.systemCursor))
    >- diffEdge SDL.SystemCursorArrow >>> mapEventS (dictCaching SDL.createSystemCursor)

  returnA -< GuiOut
    { _sdlOut = SdlOut
      { _render = do
          GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral $ winsize^.xComponent) (fromIntegral $ winsize^.yComponent))
          GL.clearColor $= GL.Color4 1 1 1 1
          GL.clear [GL.ColorBuffer, GL.StencilBuffer]
          withNvgFrame nvgctx winsize do
            drawPicture nvgctx (widget^.picture)
      , _quitReq = quit
      , _cursorReq = setcursor
      }
    , _action = do
        pass
        -- print baro
        -- when (isEvent $ btn1o^.triggered) do
        --   putStrLn "HELL OWROLD BUTTON 1"
        -- when (isEvent $ btn2o^.triggered) do
        --   putStrLn "HELL OWROLD BUTTON 2"
    }


main :: IO ()
main = do
  SDL.initializeAll
  win <- SDL.createWindow "SDL window" SDL.defaultWindow
    { SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL
    , SDL.windowResizable = True
    }
  glctx <- SDL.glCreateContext win
  SDL.glMakeCurrent win glctx
  Glew.initialize
  nvgctx  <- NVG.createGL3 $ Set.fromList [NVG.Antialias, NVG.StencilStrokes, NVG.Debug]
  nvgfont <- readFileNvgFont nvgctx "OpenSans-Regular.ttf"

  my_sf   <- newIORef (handler nvgctx nvgfont)
  my_time <- newIORef =<< SDL.time

  fix \loop -> do
    events <- SDL.pollEvents

    winsize  <- StateVar.get $ SDL.windowSize win

    old_time <- readIORef my_time
    old_sf   <- readIORef my_sf
    new_time <- SDL.time

    (guiout, new_sf) <-
      usingReaderT (new_time - old_time) $ unMSF old_sf SdlIn
        { _events = events
        , _currentWindowSize = fmap fromIntegral (winsize^.hgeometry)
        }
    liftIO $ guiout^.action

    writeIORef my_sf   new_sf
    writeIORef my_time new_time
    unless (isEvent (guiout^.sdlOut.quitReq)) do
      guiout^.sdlOut.render
      case guiout^.sdlOut.cursorReq of
        NoEvent      -> pass
        Event cursor -> SDL.activeCursor $= cursor
      SDL.glSwapWindow win
      SDL.delay (1000 `div` 60)
      loop
  SDL.quit

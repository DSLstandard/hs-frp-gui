module Vg.Widget.Scroll where

import           Control.Lens
import           Data.Colour
import           Data.Colour.Names
import           Data.Ext
import           Data.Geometry
import qualified Data.Geometry.Box as GB
import           FRP.BearRiver
import           Relude
import qualified SDL
import           Vg.Math
import           Vg.Nvg
import           Vg.Sdl
import           Vg.Widget.Widget
import Vg.Widget.Widget (Widget(_picture))

data ScrollBar
  = ScrollBar
      { _pageStep   :: Int
      , _pageLength :: Int
      , _value      :: Int
      }
  deriving (Show)
makeFieldsNoPrefix ''ScrollBar

data ScrollBarOut
  = ScrollBarOut
      { _value :: Int
      }
  deriving (Show)
makeFieldsNoPrefix ''ScrollBarOut

commonScrollBarWidth :: Int
commonScrollBarWidth = 9

dynScrollBar :: Monad m
  => SF m (GuiIn, Area Int, ScrollBar) (Widget, ScrollBarOut)
dynScrollBar = proc (guiin, area, bar) -> do
  hovering <- (guiin, area) >- dynAreaHovering
  dragging <- (guiin, area) >- dynAreaDragging
  mousepos <- (guiin^.sdlIn) >- mousePosition
  let
    valrange :: Range Int = ClosedRange 0 (bar^.pageLength - bar^.pageStep)
    valToY :: Int -> Int =
      round
      . mapRange
          (fmap fromIntegral $ ClosedRange 0 (bar^.pageLength))
          (fmap fromIntegral $ area^._Extent.yComponent)
      . fromIntegral
    ctrlYrange = ClosedRange (valToY 0) (valToY (fromIntegral (valrange^.upper.unEndPoint)))
    ctrlRange val = ClosedRange (valToY val) (valToY (val + bar^.pageStep))
    ctrlHeight = rangeLength (ctrlRange 0)
    ctrlArea :: Int -> Area Int
    ctrlArea val = GB.grow (-1) $ Vector2 (area^._Extent.xComponent) (ctrlRange val)^.from _Extent
  let
    newval = if dragging
      then round $ clampMapRange (fmap fromIntegral ctrlYrange) (fmap fromIntegral valrange) (fromIntegral (mousepos^.yCoord) - fromIntegral ctrlHeight / 2)
      else bar^.value
  let
    out = ScrollBarOut
      { _value = newval
      }
  let
    ctrlcolor
      | dragging  = lightblue
      | hovering  = lightgray
      | otherwise = whitesmoke
    pic = mconcat
      [ fill (CColor (opaque gray))       (rectangle (areaize area))
      , fill (CColor (opaque ctrlcolor)) (roundRectangle 3 (areaize (ctrlArea newval)))
      ]
  let
    widget = Widget
      { _picture = pic
      , _systemCursor = if
          | dragging  -> pure SDL.SystemCursorSizeNS
          | hovering  -> pure SDL.SystemCursorHand
          | otherwise -> mempty
      }
  returnA -< (widget, out)

------------------------------------------------------------------------
-- * ScrollBarAuto
------------------------------------------------------------------------

data ScrollBarCfg
  = ScrollBarCfg
      { _pageStep   :: Int
      , _pageLength :: Int
      }
  deriving (Show)
makeFieldsNoPrefix ''ScrollBarCfg

dynScrollBarAuto :: Monad m
  => SF m (GuiIn, Area Int, ScrollBarCfg) (Widget, ScrollBarOut)
dynScrollBarAuto = feedback 0 proc ((guiin, area, ScrollBarCfg{..}), val) -> do
  let bar = ScrollBar{_value = val, ..}
  (w, o) <- (guiin, area, bar) >- dynScrollBar
  returnA -< ((w, o), o^.value)

------------------------------------------------------------------------
-- * ScrollArea
------------------------------------------------------------------------

data ScrollArea
  = ScrollArea
      { _clientLength :: Int
      }
  deriving (Show)
makeFieldsNoPrefix ''ScrollArea

dynScrollAreaAuto :: Monad m
  => SF m (GuiIn, Area Int, input) (Widget, output)
  -> SF m (GuiIn, Area Int, ScrollArea, input) (Widget, output)
dynScrollAreaAuto dynclient = proc (guiin, area, scr, clientin) -> do
  let
    bararea = takeBox (xComponent.mirrored) commonScrollBarWidth area
    viewarea = dropBox (xComponent.mirrored) commonScrollBarWidth area
    bar = ScrollBarCfg
      { _pageStep = GB.size viewarea^.yComponent
      , _pageLength = scr^.clientLength
      }
  (barw, baro) <- (guiin, bararea, bar) >- dynScrollBarAuto
  let
    clientsize = Vector2 (GB.size viewarea^.xComponent) (scr^.clientLength)
    clientarea = fromOrgSize
      (viewarea^.GB.minP.core.GB.cwMin & yCoord -~ (baro^.value))
      clientsize
    clientguiin = guiin & cropBox %~ addCrop viewarea
  (clientw, cliento) <- (clientguiin, clientarea, clientin) >- dynclient
  let
    widget = mconcat
      [ plainPicture $ mconcat
          [ fill (CColor (opaque lightgray)) (rectangle (areaize area))
          ]
      , barw
      , clientw & picture %~ cropped (areaize viewarea)
      ]
  returnA -< (widget, cliento)
module Vg.Widget.Button where

import           Control.Lens
import           Data.Colour
import           Data.Colour.Names
import qualified Data.Geometry.Box as GB
import qualified Data.Geometry.Box as GC
import           FRP.BearRiver
import           Relude
import qualified SDL
import           Vg.Math
import           Vg.Nvg
import           Vg.Widget.Widget
import Data.Geometry

data Button
  = Button
      { _font :: VgFont
      , _text :: Text
      }
makeFieldsNoPrefix ''Button

buttonMinHeight :: Button -> Int
buttonMinHeight Button{..} = ceiling (_font^.fontSize)

buttonMinSize :: Button -> Vector 2 Int
-- buttonMinSize btn = Vector2 (ceiling $ vgFontTextSize (btn^.font) (btn^.text)^.xComponent) (buttonMinHeight btn)
buttonMinSize btn = growSize pad $ ceiling <$> vgFontTextSize (btn^.font) (btn^.text)
  where
  pad :: Int
  pad = 12

data ButtonOut
  = ButtonOut
      { _triggered :: Event ()
      }
  deriving (Show)
makeFieldsNoPrefix ''ButtonOut

dynButton :: Monad m => SF m (GuiIn, Area Int, Button) (Widget, ButtonOut)
dynButton = proc (guiin, area, btn) -> do
  hovering <- (guiin, area) >- dynAreaHovering
  dragging <- (guiin, area) >- dynAreaDragging
  (cantrigger, triggered) <- (guiin, area) >- dynAreaTriggered
  let
    out = ButtonOut
      { _triggered = triggered
      }
  let
    areacolor
      | dragging  = opaque lightblue
      | hovering  = opaque lightgray
      | otherwise = opaque whitesmoke
    bordercolor = darken 0.3 areacolor
    pic = mconcat
      [ fill   (CColor areacolor)   (rectangle (areaize area))
      , stroke (CColor bordercolor) (rectangle (borderize area))
      , fillText (CColor (opaque black)) (VgText (btn^.font) alignCenter (GB.centerPoint $ Relude.second fromIntegral area) (btn^.text))
      ]
  let
    widget = Widget
      { _picture = pic
      , _systemCursor = if
          | cantrigger == Just True  -> pure SDL.SystemCursorHand
          | cantrigger == Just False -> pure SDL.SystemCursorNo
          | hovering   -> pure SDL.SystemCursorHand
          | otherwise  -> mempty
      }
  returnA -< (widget, out)

module Vg.Widget.Field where

import Control.Lens
import FRP.BearRiver
import Relude
import Vg.Math
import Vg.Nvg
import Vg.Widget.Widget
import qualified SDL
import Data.Colour
import Data.Colour.Names
import Data.Geometry

data Field
  = Field
      { _font :: VgFont
      , _ghostText :: Maybe Text
      }
makeFieldsNoPrefix ''Field

fieldMinHeight :: Field -> Int
fieldMinHeight Field{..} = ceiling (_font^.fontSize) + pad
  where
  pad = 12

fieldLeftPad :: Int
fieldLeftPad = 1

data FieldOut
  = FieldOut
      { _text :: Text
      }
  deriving (Show)
makeFieldsNoPrefix ''FieldOut

dynField :: Monad m => SF m (GuiIn, Area Int, Field) (Widget, FieldOut)
dynField = proc (guiin, area, field) -> do
  hovering <- (guiin, area) >- dynAreaHovering
  focusing <- (guiin, area) >- dynAreaFocusing
  let
    textOf = VgText
      (field^.font)
      alignLeftCenter
      (boxFaceCenter xComponent (Relude.second fromIntegral area) & xCoord +~ fromIntegral fieldLeftPad)
  let
    out = FieldOut
      { _text = ""
      }
  let
    showghost = guard (not focusing) *> field^.ghostText
    textdisplay =
      case showghost of
        Nothing -> -- should not show ghost
          fillText (CColor (opaque darkgray)) (textOf "")
        Just txt -> -- shoud show ghost (with txt provided)
          fillText (CColor (opaque darkgray)) (textOf txt)
    bordercolor
      | focusing  = blue
      | hovering  = lightskyblue
      | otherwise = gray
    pic = mconcat
      [ fill   (CColor (opaque white))       (rectangle (areaize area))
      , stroke (CColor (opaque bordercolor)) (rectangle (borderize area))
      , textdisplay
      ]
    widget = Widget
      { _systemCursor = if hovering then pure SDL.SystemCursorIBeam else mempty
      , _picture = pic
      }
  returnA -< (widget, out)

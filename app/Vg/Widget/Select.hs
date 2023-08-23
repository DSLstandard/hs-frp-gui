module Vg.Widget.Select where

import           Control.Lens
import           Data.Colour
import           Data.Colour.Names
import           Data.Ext
import           Data.Geometry
import qualified Data.Geometry.Box as GB
import qualified Data.Stream as Stream
import qualified Data.Vector as V
import           FRP.BearRiver
import           Relude
import qualified SDL
import           Vg.Frp
import           Vg.Math
import           Vg.Nvg
import           Vg.Widget.Widget
import Vg.Widget.Scroll

data Select
  = Select
      { _font    :: VgFont
      , _choices :: V.Vector Text
      , _showMaxCount :: Maybe Int
      }
makeFieldsNoPrefix ''Select

selectRightWidth :: Int
selectRightWidth = 12

selectTriangleRadius :: Float
selectTriangleRadius = fromIntegral selectRightWidth / 3

-- TODO: REALLY BAD DESIGN
selectMinSize :: Select -> Vector 2 Int
selectMinSize Select{..} =
  (xComponent +~ selectRightWidth) $ growSize pad $ ceiling <$> vgFontTextSize _font "[Please Select]" -- should at least contain [Please Select]
  where
  pad = 3 -- default pad

data SelectOut
  = SelectOut
      { _chosen :: Maybe Text
      }
  deriving (Show)
makeFieldsNoPrefix ''SelectOut

dynSelect :: Monad m => SF m (GuiIn, Area Int, Select) (Widget, SelectOut)
dynSelect = proc (guiin, area, sel) -> do
  hovering <- (guiin, area) >- dynAreaHovering
  let focusing = True -- TODO:
  let
    newchosen :: Maybe Text = Nothing
  let
    tribox = takeBox (xComponent.mirrored) selectRightWidth area
    displaytext = fromMaybe "[Please Select]" newchosen
    pic = mconcat
      [ fill   (CColor (opaque white)) (rectangle (areaize area))
      , stroke (CColor (opaque gray))  (rectangle (borderize area))
      , fill   (CColor (opaque black)) (polygon (triangle (dirDown^*selectTriangleRadius) (GB.centerPoint $ Relude.second fromIntegral tribox)))
      , labelText (sel^.font) displaytext area
      ]
  let
    itemsize = GB.size area
    itemlay = vCatList $ (sel^.choices.to toList)
      <&> \item -> ofMinSize' itemsize \itemarea -> (itemarea, item)
    -- itemxs = layoutMinAt pickorg itemlay
    pickorg = (GB.minPoint area^.core & yCoord +~ GB.size area^.yComponent)
    pickclientheight = itemlay^.minimumSize.yComponent
    picksize
      | Just maxcount <- sel^.showMaxCount, V.length (sel^.choices) > maxcount =
          itemsize & yComponent *~ maxcount
      | otherwise =
          itemlay^.minimumSize
    pickarea = fromOrgSize pickorg picksize
  (pickwidget, pickout) <- (gateGui focusing guiin, pickarea, ScrollArea pickclientheight, (sel, itemlay)) >-
    dynScrollAreaAuto proc (guiin, clientarea, (sel, itemlay)) -> do
      let itemxs = layoutMinAt (GB.minPoint clientarea^.core) itemlay
      (unzip -> (widgets, outs)) <- fmap (sel, guiin,) itemxs >- dynamicParC
        proc (sel, guiin, (itemarea, item)) -> do
          hovering  <- (guiin, itemarea) >- dynAreaHovering
          (_, triggered) <- (guiin, itemarea) >- dynAreaTriggered
          let
            areacolor
              | hovering  = lightblue
              | otherwise = whitesmoke
            bordercolor = darken 0.95 areacolor
            pic = mconcat
              [ fill   (CColor (opaque areacolor))   (rectangle (areaize itemarea))
              , stroke (CColor (opaque bordercolor)) (rectangle (borderize itemarea))
              , labelText (sel^.font) item itemarea
              ]
            widget = Widget
              { _picture = pic
              , _systemCursor = if
                  | hovering  -> pure SDL.SystemCursorHand
                  | otherwise -> mempty
              }
          returnA -< (widget, triggered $> item)
      returnA -< (mconcat widgets, asum outs)
  let
    out = SelectOut
      { _chosen = Nothing
      }
    mainwidget = Widget
      { _picture = pic
      , _systemCursor = if
          | hovering  -> pure SDL.SystemCursorHand
          | otherwise -> mempty
      }
    widget = mainwidget <> gateWidget focusing pickwidget
  returnA -< (widget, out)

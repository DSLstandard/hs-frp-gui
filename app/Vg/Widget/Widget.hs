module Vg.Widget.Widget where

import           Control.Lens
import           Data.Ext
import           Data.Geometry
import qualified Data.Geometry.Box as GB
import           FRP.BearRiver
import           Relude
import qualified SDL
import           Vg.Math
import           Vg.Nvg
import           Vg.Sdl
import Data.Colour

data GuiIn
  = GuiIn
      { _cropBox :: CropBox 2 Int
      , _sdlIn   :: SdlIn
      }
  deriving (Show)
makeFieldsNoPrefix ''GuiIn

disabledGui :: GuiIn -> GuiIn
disabledGui = cropBox .~ CropBox'Empty -- force set to Empty to achieve the effect

gateGui :: Bool -> GuiIn -> GuiIn
gateGui False = disabledGui
gateGui True  = id

------------------------------------------------------------------------
-- * Widget
------------------------------------------------------------------------

data Widget
  = Widget
      { _picture      :: Picture
      , _systemCursor :: Last SDL.SystemCursor
      }
makeFieldsNoPrefix ''Widget

gateWidget :: Bool -> Widget -> Widget
gateWidget False = const mempty
gateWidget True  = id

plainPicture :: Picture -> Widget
plainPicture pic = Widget
  { _picture = pic
  , _systemCursor = mempty
  }

instance Semigroup Widget where
  Widget a1 a2 <> Widget b1 b2 = Widget (a1 <> b1) (a2 <> b2)

instance Monoid Widget where
  mempty = Widget mempty mempty

------------------------------------------------------------------------
-- * FRP
------------------------------------------------------------------------

dynAreaHovering :: Monad m => SF m (GuiIn, Area Int) Bool
dynAreaHovering = hold False <<< proc (guiin, area_in) -> do
  let area = applyCrop (guiin^.cropBox) area_in
  mousepos <- guiin^.sdlIn >- mousePosition
  returnA -< case area of
    Nothing   -> NoEvent
    Just area -> Event (GB.inBox mousepos area)

dynAreaDragging :: Monad m => SF m (GuiIn, Area Int) Bool
dynAreaDragging = hold False <<< proc (guiin, area_in) -> do
  hovering <- (guiin, area_in) >- dynAreaHovering
  pressed  <- (guiin^.sdlIn) >- mousePressed SDL.ButtonLeft
  released <- (guiin^.sdlIn) >- mouseReleased SDL.ButtonLeft
  returnA -< if
    | hovering, isEvent pressed -> Event True
    | isEvent released -> Event False
    | otherwise -> NoEvent

dynAreaTriggered :: Monad m => SF m (GuiIn, Area Int) (Maybe Bool, Event ()) -- ^ (able to trigger if dragging, trigger event)
dynAreaTriggered = proc (guiin, area_in) -> do
  hovering  <- (guiin, area_in) >- dynAreaHovering
  dragging  <- (guiin, area_in) >- dynAreaDragging
  triggered <- not dragging >- edgeFrom False
  returnA -< (guard dragging $> hovering, triggered `gate` hovering)

dynAreaFocusing :: Monad m => SF m (GuiIn, Area Int) Bool
dynAreaFocusing = hold False <<< proc (guiin, area_in) -> do
  (_, triggered) <- (guiin, area_in) >- dynAreaTriggered
  pressed   <- (guiin^.sdlIn) >- mousePressed SDL.ButtonLeft
  returnA -< if
    | isEvent triggered -> Event True
    | isEvent pressed   -> Event False
    | otherwise         -> NoEvent

------------------------------------------------------------------------
-- * Lay
------------------------------------------------------------------------

data Lay a
  = Lay
      { _buildFromBox :: Area Int -> a
      , _minimumSize  :: Vector 2 Int
      }
  deriving (Functor)
makeFieldsNoPrefix ''Lay

layoutMinAt :: Point 2 Int -> Lay a -> a
layoutMinAt org Lay{..} = _buildFromBox (fromOrgSize org _minimumSize)

ofMinSize :: Vector 2 Int -> Lay (Area Int)
ofMinSize sz = Lay id sz

ofMinSize' :: Vector 2 Int -> (Area Int -> a) -> Lay a
ofMinSize' sz f = Lay f sz

-- catList :: (forall a b. Lay (a -> b) -> Lay a -> Lay b) -> Lay r -> [Lay r] -> Lay [r]
-- catList combine x [] = fmap pure x
-- catList combine x (y : ys) = catList combine (combine (fmap _ x) y) ys

vCatList' :: Lay [r] -> [Lay r] -> Lay [r]
vCatList' = foldl' \lya -> vAp (lya <&> \rs r -> rs <> [r])

vCatList :: [Lay r] -> Lay [r]
vCatList = vCatList' $ vPure (const [])

----- ============================================================ -----
-- ** Vertical
----- ============================================================ -----

vPure :: (Area Int -> a) -> Lay a
vPure f = Lay f (Vector2 0 0)

infixl 4 `vAp`

vAp :: Lay (a -> b) -> Lay a -> Lay b
vAp lyf lya = Lay
  { _buildFromBox = \area -> do
      let org0 :+ () = GB.minPoint area
      let org1 = org0 & yCoord +~ lyf^.minimumSize.yComponent
      let f = layoutMinAt org0 lyf
      let a = layoutMinAt org1 lya
      f a
  , _minimumSize = Vector2 max (+) <*> (lyf^.minimumSize) <*> (lya^.minimumSize)
  }

----- ============================================================ -----
-- ** Horizontal
----- ============================================================ -----

hPure :: (Area Int -> a) -> Lay a
hPure f = Lay f (Vector2 0 0)

infixl 4 `hAp`

hAp :: Lay (a -> b) -> Lay a -> Lay b
hAp lyf lya = Lay
  { _buildFromBox = \area -> do
      let org0 :+ () = GB.minPoint area
      let org1 = org0 & xCoord +~ lyf^.minimumSize.xComponent
      let f = layoutMinAt org0 lyf
      let a = layoutMinAt org1 lya
      f a
  , _minimumSize = Vector2 (+) max <*> (lyf^.minimumSize) <*> (lya^.minimumSize)
  }

------------------------------------------------------------------------
-- * Pictures
------------------------------------------------------------------------

labelText :: VgFont -> Text -> Area Int -> Picture
labelText font txt area = mconcat
  [ fillText (CColor (opaque black))
      (VgText font alignLeftCenter (boxFaceCenter xComponent (areaize area) & xCoord +~ leftpad) txt)
  ]
  where
  leftpad = 1 -- add some left pad to make the text be not touching the left wall so intimately
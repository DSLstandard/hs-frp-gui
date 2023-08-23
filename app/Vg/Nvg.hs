module Vg.Nvg where

import           Control.Lens
import           Data.Colour
import           Data.Colour.RGBSpace
import           Data.Colour.SRGB
import           Data.Ext
import           Data.Geometry
import           Data.Geometry.Box
import qualified Data.Geometry.Box as GB
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Text.Foreign (withCStringLen)
import qualified Data.Vector as V
import           Foreign.Ptr
import qualified NanoVG as NVG
import           Relude
import qualified System.IO.Unsafe as Unsafe
import           Vg.Math

------------------------------------------------------------------------
-- * Utils
------------------------------------------------------------------------

withNvgFrame :: MonadIO m => NVG.Context -> Vector 2 Int -> m a -> m a
withNvgFrame nvgctx (Vector2 w h) act =
  liftIO (NVG.beginFrame nvgctx (fromIntegral w) (fromIntegral h) 1)
  *> act <* liftIO (NVG.endFrame nvgctx)

withNvgState :: MonadIO m => NVG.Context -> m a -> m a
withNvgState nvgctx act = do
  liftIO (NVG.save nvgctx) *> act <* liftIO (NVG.restore nvgctx)

withDefNvgState :: MonadIO m => NVG.Context -> m a -> m a
withDefNvgState nvgctx act =
  liftIO (NVG.save nvgctx) *> act <* liftIO (NVG.restore nvgctx)

------------------------------------------------------------------------
-- * Shape
------------------------------------------------------------------------

newtype Shape
  = Shape { unShape :: NVG.Context -> IO () }
  deriving (Monoid, Semigroup)

setShape :: MonadIO m => NVG.Context -> Shape -> m ()
setShape nvgctx (Shape act) = liftIO $ do
  NVG.beginPath nvgctx *> act nvgctx

openPath :: [Point 2 Float] -> Shape
openPath [] = mempty
openPath (p : ps) = Shape \nvgctx -> do
  NVG.moveTo nvgctx (realToFrac (p^.xCoord)) (realToFrac (p^.yCoord))
  forM_ ps \p ->
    NVG.lineTo nvgctx (realToFrac (p^.xCoord)) (realToFrac (p^.yCoord))

closedPath :: [Point 2 Float] -> Shape
closedPath [] = mempty
closedPath (p : ps) = Shape \nvgctx -> do
  NVG.moveTo nvgctx (realToFrac (p^.xCoord)) (realToFrac (p^.yCoord))
  forM_ ps \p ->
    NVG.lineTo nvgctx (realToFrac (p^.xCoord)) (realToFrac (p^.yCoord))
  NVG.lineTo nvgctx (realToFrac (p^.xCoord)) (realToFrac (p^.yCoord))

line :: LineSegment 2 p Float -> Shape
line (LineSegment' (start :+ _) (end :+ _)) =
  openPath [start, end]

-- regularPolygon ::
--   Int -- ^Number of sides
--   -> Vector 2 Float -- ^Radius pointer
--   -> Point 2 Float
--   -> Shape
-- regularPolygon nsides delta org =
--   closedPath (take nsides ps)
--   where
--   ps = [ org & vector %~ (^+^ ptr) | ptr <- ptrs ]
--   ptrs = delta : fmap (transformBy mat) ptrs
--   mat = rotation (tau / fromIntegral nsides)

borderize :: Box 2 p Int -> Box 2 p Float
-- borderize = grow (-0.5) . second realToFrac
borderize = displaceBox (Vector2 0.5 0.5) . second realToFrac

areaize :: Box 2 p Int -> Box 2 p Float
areaize = second realToFrac

rectangle :: Box 2 p Float -> Shape
rectangle bx = Shape \ctx -> do
  let (Point2 x y, Vector2 w h) = toOrgSize bx
  NVG.rect ctx (realToFrac x) (realToFrac y) (realToFrac w) (realToFrac h)

roundRectangle :: Float -> Box 2 p Float -> Shape
roundRectangle radius bx = Shape \ctx -> do
  let (Point2 x y, Vector2 w h) = toOrgSize bx
  NVG.roundedRect ctx (realToFrac x) (realToFrac y) (realToFrac w) (realToFrac h) (realToFrac radius)

polygon :: SimplePolygon p Float -> Shape
polygon = closedPath . fmap (view core) . toPoints

------------------------------------------------------------------------
-- * Color
------------------------------------------------------------------------

toNvgRGB :: Colour Float -> NVG.Color
toNvgRGB col = do
  let (fmap realToFrac -> RGB r g b) = toSRGB col
  NVG.rgbf r g b

toNvgRGBA :: AlphaColour Float -> NVG.Color
toNvgRGBA ac
  | a > 0 =
      let (fmap realToFrac -> RGB r g b) = toSRGB $ darken (recip a) (ac `Data.Colour.over` black)
      in NVG.rgbaf r g b (realToFrac a)
  | otherwise = NVG.rgba 0 0 0 0
  where
  a = alphaChannel ac

------------------------------------------------------------------------
-- * Picture
------------------------------------------------------------------------

newtype Picture
  = Picture { unPicture :: NVG.Context -> IO () }
  deriving (Monoid, Semigroup)

drawPicture :: MonadIO m => NVG.Context -> Picture -> m ()
drawPicture nvgctx (Picture act) = liftIO $ act nvgctx

data CPaint
  = CColor (AlphaColour Float)

type StrokePaint = CPaint
type FillPaint = CPaint

setFillPaint :: MonadIO m => NVG.Context -> StrokePaint -> m ()
setFillPaint nvgctx (CColor col) = do
  liftIO $ NVG.fillColor nvgctx (toNvgRGBA col)

setStrokePaint :: MonadIO m => NVG.Context -> FillPaint -> m ()
setStrokePaint nvgctx (CColor col) = do
  liftIO $ NVG.strokeColor nvgctx (toNvgRGBA col)

fill :: FillPaint -> Shape -> Picture
fill p shape = Picture \nvgctx -> do
  withNvgState nvgctx do
    setFillPaint nvgctx p
    setShape nvgctx shape
    liftIO $ NVG.fill nvgctx

stroke :: StrokePaint -> Shape -> Picture
stroke p shape = Picture \nvgctx -> do
  withNvgState nvgctx do
    setStrokePaint nvgctx p
    setShape nvgctx shape
    liftIO $ NVG.stroke nvgctx

cropped :: Area Float -> Picture -> Picture
cropped area pic = Picture \nvgctx -> do
  withNvgState nvgctx do
    let (Point2 x y, Vector2 w h) = toOrgSize area
    NVG.intersectScissor nvgctx
      (realToFrac x) (realToFrac y)
      (realToFrac w) (realToFrac h)
    drawPicture nvgctx pic

visibleWhen :: Bool -> Picture -> Picture
visibleWhen = bool mempty id

hiddenWhen :: Bool -> Picture -> Picture
hiddenWhen = bool id mempty

------------------------------------------------------------------------
-- * Font
------------------------------------------------------------------------

data NvgFont
  = NvgFont
      { _internalContext :: NVG.Context
        -- ^Needed for computations
      , _internalFont    :: NVG.Font
      }
makeFieldsNoPrefix ''NvgFont

readFileNvgFont :: (MonadIO m, MonadFail m) => NVG.Context -> Text -> m NvgFont
readFileNvgFont nvgctx path = do
  Just font <- liftIO $ NVG.createFont nvgctx ("loadNvgFont!@#$_" <> path) (NVG.FileName path)
  pure (NvgFont nvgctx font)

data VgFont
  = VgFont
      { _nvgFont  :: NvgFont
      , _fontSize :: Float
      }
makeFieldsNoPrefix ''VgFont

setTextFont :: MonadIO m => NVG.Context -> VgFont -> m ()
setTextFont nvgctx (VgFont f s) = do
  liftIO $ NVG.fontFaceId nvgctx (f^.internalFont)
  liftIO $ NVG.fontSize   nvgctx (realToFrac s)

reallyUnsafeWithVgFont :: VgFont -> (NVG.Context -> IO a) -> a
reallyUnsafeWithVgFont vgfont@VgFont{..} act = do
  let nvgctx = _nvgFont^.internalContext
  Unsafe.unsafePerformIO do
    withDefNvgState nvgctx do
      setTextFont nvgctx vgfont
      act nvgctx

vgFontTextSize :: VgFont -> Text -> Vector 2 Float
vgFontTextSize vgfont txt =
  reallyUnsafeWithVgFont vgfont \nvgctx -> do
    NVG.Bounds (fmap realToFrac -> NVG.V4 xmin ymin xmax ymax) <- NVG.textBounds nvgctx 0 0 txt
    pure $ GB.size $ GB.box (ext $ Point2 xmin ymin) (ext $ Point2 xmax ymax)

data TextMetrics
  = TextMetrics
      { _ascender   :: Float
        -- ^ascender offset from baseline under subtraction
      , _descender  :: Float
        -- ^descender offset from baseline under subtraction
      , _lineHeight :: Float
      }
  deriving (Show)
makeFieldsNoPrefix ''TextMetrics

vgFontTextMetrics :: VgFont -> TextMetrics
vgFontTextMetrics vgfont =
  reallyUnsafeWithVgFont vgfont \nvgctx -> do
    (asc, des, lineh) <- NVG.textMetrics nvgctx
    pure $ TextMetrics (realToFrac asc) (realToFrac des) (realToFrac lineh)

----- ============================================================ -----
-- ** Align
----- ============================================================ -----

data HAlign = H'Left | H'Center | H'Right deriving (Enum, Eq, Show)

data VAlign = V'Top | V'Baseline | V'Middle | V'Bottom deriving (Enum, Eq, Show)

fromHAlign :: HAlign -> NVG.Align
fromHAlign H'Left   = NVG.AlignLeft
fromHAlign H'Center = NVG.AlignCenter
fromHAlign H'Right  = NVG.AlignRight

fromVAlign :: VAlign -> NVG.Align
fromVAlign V'Top      = NVG.AlignTop
fromVAlign V'Baseline = NVG.AlignBaseline
fromVAlign V'Middle   = NVG.AlignMiddle
fromVAlign V'Bottom   = NVG.AlignBottom

data Align
  = Align
      { _halign :: HAlign
      , _valign :: VAlign
      }
  deriving (Eq, Show)
makeFieldsNoPrefix ''Align

alignCenter = Align H'Center V'Middle
alignLeftCenter = Align H'Left V'Middle

fromAlign :: Align -> Set.Set NVG.Align
fromAlign (Align h v) = Set.fromList [fromHAlign h, fromVAlign v]

setTextAlign :: MonadIO m => NVG.Context -> Align -> m ()
setTextAlign nvgctx align =
  liftIO $ NVG.textAlign nvgctx (fromAlign align)

------------------------------------------------------------------------
-- * Text
------------------------------------------------------------------------

data VgText
  = VgText
      { _font      :: VgFont
      , _alignment :: Align
      , _position  :: Point 2 Float
      , _text      :: Text
      }
makeFieldsNoPrefix ''VgText

data VgGlyph
  = VgGlyph
      { _xRange :: Range Float
      }
makeFieldsNoPrefix ''VgGlyph

vgTextGlyphs :: VgText -> V.Vector VgGlyph
vgTextGlyphs VgText{..}
  | T.null _text = mempty -- catch text empty cases to not cause a segfault from
  | otherwise =
      reallyUnsafeWithVgFont _font \ctx -> do
        let (fmap realToFrac -> Point2 x y) = _position
        withCStringLen _text \(ptr, bytelen) -> do
          glyphs <- NVG.textGlyphPositions ctx x y
            ptr (ptr `plusPtr` bytelen) (fromIntegral (T.length _text))
          pure $ glyphs <&> \g ->
            VgGlyph (ClosedRange (realToFrac (NVG.glyphX g)) (realToFrac (NVG.glyphPosMaxX g)))

vgTextBaselineY :: VgText -> Float
vgTextBaselineY VgText{..} = do
  let sy = _position^.yCoord
  let TextMetrics asc dsc lineh = vgFontTextMetrics _font
  case _alignment^.valign of
    V'Baseline -> sy
    V'Top      -> sy - asc
    V'Bottom   -> sy - dsc
    V'Middle   -> sy - (asc + dsc) / 2

type CaretPos = Int
type CaretX = Int

vgTextUnboundedXToCaret :: VgText -> CaretX -> CaretPos
vgTextUnboundedXToCaret vgtext@VgText{..} (realToFrac -> x_in) = do -- need not to handle text empty case, as it falls back to the Nothing branch
  let gs = vgTextGlyphs vgtext
  case ifind (\_ g -> inRange x_in (g^.xRange)) gs of
    Nothing -> do -- beyond the boundary
      bool 0 (T.length _text) (_position^.xCoord < x_in)
    Just (unbiasedcaret, g) ->
      if x_in <= centerValue (g^.xRange)
        then unbiasedcaret
        else unbiasedcaret + 1

vgTextCaretToX :: VgText -> CaretPos -> CaretX
vgTextCaretToX vgtext@VgText{..} caret
  | caret == 0 =
      ceiling (_position^.xCoord)
  | otherwise =
      ceiling $ vgTextGlyphs vgtext^?!ix caret.xRange.upper.unEndPoint

fillText :: FillPaint -> VgText -> Picture
fillText paint VgText{..} =
  hiddenWhen (T.null _text) do
    Picture \nvgctx -> do
      unless (T.null _text) do -- prevent segfault
        withNvgState nvgctx do
          setTextFont  nvgctx _font
          setTextAlign nvgctx _alignment
          setFillPaint nvgctx paint
          liftIO $ NVG.text nvgctx
            (realToFrac (_position^.xCoord)) (realToFrac (_position^.yCoord))
            _text

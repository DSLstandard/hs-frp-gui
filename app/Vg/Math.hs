module Vg.Math where

import           Control.Lens
import           Data.Ext
import           Data.Geometry
import           Data.Geometry.Box
import qualified Data.Geometry.Box as GB
import qualified Data.Set as Set
import           Data.Stream
import           Data.Vector.Circular (iterateN1)
import           Data.Vinyl
import           Data.Vinyl.CoRec
import qualified Linear as L
import qualified Linear.Affine as L
import qualified NanoVG as NVG
import           Relude
import qualified System.IO.Unsafe as Unsafe

type Area a = Box 2 () a

tau :: Floating a => a
tau = pi * 2

class IsoGeo f g | f -> g where
  hgeometry :: Iso (f a) (f b) (g a) (g b)

instance IsoGeo L.V1 (Vector 1) where
  hgeometry = iso
    (\(L.V1    a) -> Vector1 a)
    (\(Vector1 a) -> L.V1    a)

instance IsoGeo L.V2 (Vector 2) where
  hgeometry = iso
    (\(L.V2    a b) -> Vector2 a b)
    (\(Vector2 a b) -> L.V2    a b)

instance IsoGeo L.V3 (Vector 3) where
  hgeometry = iso
    (\(L.V3    a b c) -> Vector3 a b c)
    (\(Vector3 a b c) -> L.V3    a b c)

instance IsoGeo L.V4 (Vector 4) where
  hgeometry = iso
    (\(L.V4    a b c d) -> Vector4 a b c d)
    (\(Vector4 a b c d) -> L.V4    a b c d)

instance IsoGeo (L.Point L.V1) (Point 1) where
  hgeometry = iso
    (\(L.P (L.V1 a)) -> Point1    a)
    (\(Point1    a)  -> L.P (L.V1 a))

instance IsoGeo (L.Point L.V2) (Point 2) where
  hgeometry = iso
    (\(L.P (L.V2 a b)) -> Point2    a b)
    (\(Point2    a b)  -> L.P (L.V2 a b))

instance IsoGeo (L.Point L.V3) (Point 3) where
  hgeometry = iso
    (\(L.P (L.V3 a b c)) -> Point3    a b c)
    (\(Point3    a b c)  -> L.P (L.V3 a b c))

-- instance IsoGeo (L.Point L.V4) (Point 4) where
--   hgeometry = iso
--     (\(L.P (L.V4 a b c d)) -> Point4    a b c d)
--     (\(Point4    a b c d)  -> L.P (L.V4 a b c d))

regularPolygon :: Floating r => Int -> Vector 2 r -> Point 2 r -> SimplePolygon () r
regularPolygon nsides delta org =
  unsafeFromCircularVector $ (\ptr -> ext $ org & vector %~ (^+^ ptr)) <$> iterateN1 nsides (transformBy mat) delta
  where
  -- ps = [ org & vector %~ (^+^ ptr) | ptr <- ptrs ]
  -- ptrs = delta : fmap (transformBy mat) ptrs
  mat = rotation (tau / fromIntegral nsides)

dirDown :: Num r => Vector 2 r
dirDown = Vector2 0 1

triangle :: Floating r => Vector 2 r -> Point 2 r -> SimplePolygon () r
triangle = regularPolygon 3

fromOrgSize :: (Arity d, Num r) => Point d r -> Vector d r -> Box d () r
fromOrgSize org sz = do
  GB.box (ext org) (ext $ org .+^ fmap (subtract 1) sz)

toOrgSize :: (Arity d, Num r) => Box d p r -> (Point d r, Vector d r)
toOrgSize bx = do
  let (p :+ _, s) = (GB.minPoint bx, GB.size bx)
  (p, s)

displaceBox :: (Arity d, Num r) => Vector d r -> Box d p r -> Box d p r
displaceBox v = (GB.minP . core . cwMin %~ (.+^ v)) . (GB.maxP . core . cwMax %~ (.+^ v))

mapRange :: Fractional r => Range r -> Range r -> r -> r
mapRange a b = exportRange b . importRange a

clampMapRange :: (Fractional r, Ord r) => Range r -> Range r -> r -> r
clampMapRange a b = clampExportRange b . importRange a

rangeLength :: Num r => Range r -> r
rangeLength (Range' l r) = r - l + 1

importRange :: Fractional r => Range r -> r -> r
importRange (Range' al ar) x = (x - al) / (ar - al)

exportRange :: Fractional r => Range r -> r -> r
exportRange (Range' bl br) x = x * (br - bl) + bl

clampExportRange :: (Fractional r, Ord r) => Range r -> r -> r
clampExportRange range x = clampTo range $ exportRange range x

_Extent :: Arity d => Iso' (Box d () r) (Vector d (Range r))
_Extent = iso GB.extent GB.fromExtent

mirrored :: Num r => Iso' (Range r) (Range r)
mirrored = involuted (\(Range l r) -> Range (fmap negate r) (fmap negate l))

type Extrude d = forall k. Lens' (Vector d k) k
type ExtrudeRange d r = Lens' (Vector d (Range r)) (Range r)

takeBox :: (Arity d, Num r) => ExtrudeRange d r -> r -> Box d () r -> Box d () r
takeBox ax sz b = b & _Extent.ax.upper.unEndPoint .~ (b^._Extent.ax.lower.unEndPoint + sz)

dropBox :: (Arity d, Num r) => ExtrudeRange d r -> r -> Box d () r -> Box d () r
-- dropBox ax = takeBox (ax.mirrored)
dropBox ax sz b = b & _Extent.ax.lower.unEndPoint +~ sz

boxFace :: (Arity d) => ExtrudeRange d r -> Box d () r -> Box d () r
-- defining from takeBox additionally requires (Num r)
boxFace ax b = b & _Extent.ax.upper.unEndPoint .~ b^._Extent.ax.lower.unEndPoint

boxFaceCenter :: (Arity d, Fractional r) => ExtrudeRange d r -> Box d () r -> Point d r
boxFaceCenter ax b = GB.centerPoint $ boxFace ax b

growSize :: (Arity d, Num r) => r -> Vector d r -> Vector d r
growSize r = fmap (+ 2 * r)

teleportBox :: (Arity d, Num r) => Extrude d -> Box d p r -> Box d p r
teleportBox ax b = pmap (vector.ax +~ GB.size b^.ax) b

towerBox :: (Arity d, Num r) => Extrude d -> Box d () r -> Stream (Box d () r)
towerBox ax b =
  b' <:> towerBox ax b'
  where
  b' = teleportBox ax b

-- | Engineering sin (period ~ 1, codomain ~ [0..1])
engsin :: Floating a => a -> a
engsin x = (1 + sin (tau * x)) / 2

centerValue :: Fractional r => Range r -> r
centerValue (Range' l r) = (l + r) / 2

------------------------------------------------------------------------
-- * CropBox
------------------------------------------------------------------------

data CropBox d r
  = CropBox'Empty
  | CropBox (Box d () r)
  | CropBox'Inf

deriving instance (Show r, Arity d) => Show (CropBox d r)
deriving instance (Eq r, Arity d) => Eq (CropBox d r)

addCrop :: (Arity d, Ord r) => Box d () r -> CropBox d r -> CropBox d r
addCrop b CropBox'Empty = CropBox'Empty
addCrop b CropBox'Inf   = CropBox b
addCrop b (CropBox b')  = match (b `intersect` b')
   $ H (\NoIntersection -> CropBox'Empty)
  :& H CropBox
  :& RNil

applyCrop :: (Arity d, Ord r) => CropBox d r -> Box d () r -> Maybe (Box d () r)
applyCrop CropBox'Empty b = empty
applyCrop CropBox'Inf b = pure b
applyCrop (CropBox b) b' = match (b `intersect` b')
   $ H (\NoIntersection -> empty)
  :& H pure
  :& RNil

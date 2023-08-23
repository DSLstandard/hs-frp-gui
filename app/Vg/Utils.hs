module Vg.Utils where

import Relude
import Control.Lens
import qualified Data.Stream as S

unzipF :: Functor f => f (a, b) -> (f a, f b)
unzipF abs = (fst <$> abs, snd <$> abs)

instance FunctorWithIndex Int S.Stream where
  imap f = go 0
    where
    go i (x `S.Cons` xs) = f i x `S.Cons` go (i+1) xs

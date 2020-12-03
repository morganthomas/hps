-- | 'Path' statistics.
module Graphics.PS.Statistics where

import Data.Monoid {- base -}

import Graphics.PS.Image
import Graphics.PS.Path

-- | Path statistics data type.
data Statistics = Statistics {nMoveTo :: Integer
                             ,nLineTo :: Integer
                             ,nCurveTo :: Integer
                             ,nClosePath :: Integer
                             ,nGlyph :: Integer
                             ,nTransform :: Integer}
                  deriving (Eq,Show)

instance Monoid Statistics where
    mempty = Statistics 0 0 0 0 0 0
    mappend = statMappend

instance Semigroup Statistics where
    (<>) = statMappend

statMappend p q =
        let (Statistics m1 l1 c1 f1 g1 t1) = p
            (Statistics m2 l2 c2 f2 g2 t2) = q
        in Statistics (m1+m2) (l1+l2) (c1+c2) (f1+f2) (g1+g2) (t1+t2)

-- | Determine number of path components of each type.
pathStatistics :: Path -> Statistics
pathStatistics path =
    case path of
      MoveTo _ -> Statistics 1 0 0 0 0 0
      LineTo _ -> Statistics 0 1 0 0 0 0
      CurveTo _ _ _ -> Statistics 0 0 1 0 0 0
      ClosePath _ -> Statistics 0 0 0 1 0 0
      Text _ s -> Statistics 0 0 0 0 (fromIntegral (length s)) 0
      PTransform _ p -> Statistics 0 0 0 0 0 1 <> pathStatistics p
      Join p1 p2 -> pathStatistics p1 <> pathStatistics p2

-- | Statistics for all 'paths' at 'Image'.
imageStatistics :: Image -> Statistics
imageStatistics = mconcat . map pathStatistics . paths

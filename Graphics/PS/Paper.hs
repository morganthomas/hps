-- | Paper and BBox.
module Graphics.PS.Paper where

import Graphics.PS.Unit

-- | Enumeration type for Units.
data Units = MM | PT deriving (Eq,Show)

-- | Paper size data type, the units are specified in the type.
-- The units in postscript are points, we often work in millimeters.
data Paper = Paper {units :: Units
                   ,width :: Int
                   ,height :: Int}
             deriving (Eq,Show)

-- | Translate 'Paper' from mm to points.
--
-- > let a4 = Paper MM 210 297
-- > paper_size_pt a4 == (595,841)
paper_size_pt :: Paper -> (Int,Int)
paper_size_pt (Paper unit w h) =
    case unit of
      MM -> (mm_pt_int w,mm_pt_int h)
      PT -> (w,h)

-- | BoundingBox for an EPSF file with an optional HiResBoundingBox
data BBox = BBox {llx :: Int -- ^ lower left x (x-min)
                 ,lly :: Int -- ^ lower left y (y-min)
                 ,urx :: Int -- ^ upper right x (x-max)
                 ,ury :: Int -- ^ upper right y (y-max)
                 }
          | HRBBox {llx :: Int
                   ,lly :: Int
                   ,urx :: Int
                   ,ury :: Int
                   ,hrllx :: Double -- ^ high resolution 'llx'
                   ,hrlly :: Double -- ^ high resolution 'lly'
                   ,hrurx :: Double -- ^ high resolution 'urx'
                   ,hrury :: Double -- ^ high resolution 'ury'
                   }
            deriving (Eq,Show)

-- | Swap width and height of 'Paper'.
landscape :: Paper -> Paper
landscape (Paper u w h) = Paper u h w

-- | Proportion of 'Paper'.
--
-- > import Graphics.PS.Paper.Names
-- > proportion broadsheet == 1.25
-- > map (round . (* 1e3) . proportion) [a0,b0,c0] == [1414,1414,1414]
-- > map proportion [usLetter,berliner,tabloid]
proportion :: Paper -> Double
proportion (Paper _ w h) = i_to_d h / i_to_d w

-- | The margin given by placing paper size /p/ within /q/.
inset_margin :: Paper -> Paper -> (Units,Int,Int)
inset_margin (Paper u1 w1 h1) (Paper u2 w2 h2) =
    if u1 /= u2
    then error "inset_margin: unequal units"
    else (u2,(w2 - w1) `div` 2,(h2 - h1) `div` 2)

-- * BBox

-- | Sum of 'BBox'.
--
-- > bbox_sum (BBox 5 5 10 10) (BBox 0 0 2 20) == BBox 0 0 10 20
bbox_sum :: BBox -> BBox -> BBox
bbox_sum p q =
    case (p,q) of
      (BBox x0 y0 x1 y1,BBox x2 y2 x3 y3) ->
          BBox (min x0 x2) (min y0 y2) (max x1 x3) (max y1 y3)
      _ -> error "bbox_sum: HR not handled"

-- | Translate 'BBox' from mm to points.
bbox_mm_to_pt :: BBox -> BBox
bbox_mm_to_pt bb =
    case bb of
      BBox x y x' y' ->
          BBox (mm_pt_int x) (mm_pt_int y) (mm_pt_int x') (mm_pt_int y')
      HRBBox x y x' y' hx hy hx' hy' ->
          HRBBox (mm_pt_int x) (mm_pt_int y) (mm_pt_int x') (mm_pt_int y')
                 (mm_pt hx) (mm_pt hy) (mm_pt hx') (mm_pt hy')

{- | A 'div' variant that rounds rather than truncates.

> let f (Paper _ h _) = h `div` 2 == h `divRound` 2
> in all id (map f [b0,b1,b2,b3,b4,b5,b6,b7,b8,b9]) == False

-}
divRound :: Int -> Int -> Int
divRound x y =
    let x' = (fromIntegral x)::Double
        y' = (fromIntegral y)::Double
    in round (x' / y')

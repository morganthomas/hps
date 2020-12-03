-- | Set of predefined 'Path's.
module Graphics.PS.Path.Graphs where

import Data.CG.Minus {- hcg-minus -}
import Graphics.PS.Path
import Graphics.PS.Transform
import Graphics.PS.Unit

-- | See <ftp.scsh.net/pub/scsh/contrib/fps/doc/examples/fractal-sqr.html>
fractal_sqr_pt :: Pt Double -> Pt Double -> Int -> [Ln Double]
fractal_sqr_pt p1 p2 d =
    case d of
      0 -> [Ln p1 p2]
      _ -> let (Pt x1 y1) = p1
               (Pt x2 y2) = p2
               x3 = ((x1 + x2) / 2) + ((y2 - y1) / 2)
               y3 = ((y1 + y2) / 2) - ((x2 - x1) / 2)
               p3 = Pt x3 y3
           in fractal_sqr_pt p1 p3 (d - 1) ++ fractal_sqr_pt p3 p2 (d - 1)

-- | 'Path' of 'fractal_sqr_pt' with inputs @(250,250)@, @(175,175)@, @12@.
fractal_sqr :: Path
fractal_sqr = renderLines (fractal_sqr_pt (Pt 250 250) (Pt 175 175) 12)

-- | 'renderLines_jn' variant of 'fractal_sqr'.
fractal_sqr' :: Path
fractal_sqr' = renderLines_jn (fractal_sqr_pt (Pt 250 250) (Pt 175 175) 12)

-- | A /unit/ arrow.
unitArrow :: Int -> Path
unitArrow d =
    case d of
      1 -> MoveTo (Pt 0 0) +++ LineTo (Pt 0 1)
      _ -> let s = 0.6
               sa = scale s s (unitArrow (d - 1))
               cw = negate (radians 135)
               ccw = negate cw
           in unitArrow 1 +++
              (translate 0 1 . rotate cw) sa +++
              (translate 0 1 . rotate ccw) sa

-- | See <ftp.scsh.net/pub/scsh/contrib/fps/doc/examples/fractal-arrow.html>
fractalArrow :: Double -> Int -> Path
fractalArrow h d =
    let x = (576 - h) / 2 + h / 2
        y = (720 - h) / 2
        a = unitArrow d
    in (translate x y . scale h h) a

-- | Isosceles right angled triangle
erat :: Pt Double -> Double -> Path
erat (Pt x y) n = polygon [Pt x y,Pt (x+n) y,Pt x (y+n)]

-- | Sierpinski triangle.
sierpinski :: Pt Double -> Double -> Double -> Path
sierpinski p n limit =
    let m = n / 2
        (Pt x y) = p
        s q = sierpinski q m limit
        t1 = s p
        t2 = s (Pt x (y + m))
        t3 = s (Pt (x + m) y)
    in if n <= limit
       then erat p n
       else t1 +++ t2 +++ t3


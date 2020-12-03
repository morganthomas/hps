-- | 'Path' query functions and related operations.
module Graphics.PS.Query where

import Data.CG.Minus.Types {- hcg-minus -}
import qualified Data.CG.Minus as CG {- hcg-minus -}
import qualified Data.CG.Minus.Bezier as CG {- hcg-minus -}

import Graphics.PS.Paper
import Graphics.PS.Path

-- | Locate the starting point of the path, which must begin with a 'MoveTo' node.
startPt :: Path -> Maybe (Pt Double)
startPt path =
    case path of
      MoveTo p -> Just p
      Join p _ -> startPt p
      _ -> Nothing

-- | Variant that allows the initial node to be a 'LineTo' or 'CurveTo' node.
startPt' :: Path -> Maybe (Pt Double)
startPt' path =
    case path of
      MoveTo p -> Just p
      LineTo p -> Just p
      CurveTo p _ _ -> Just p
      Join p _ -> startPt' p
      _ -> Nothing

-- | Ensure path begins with a 'MoveTo' node.
mkValid :: Path -> Path
mkValid path =
    case startPt' path of
      Just p -> MoveTo p +++ path
      Nothing -> path

-- | Locate the end point of the path.
endPt :: Path -> Maybe (Pt Double)
endPt path =
    case path of
      MoveTo p -> Just p
      LineTo p -> Just p
      CurveTo _ _ p -> Just p
      Join _ p -> endPt p
      _ -> Nothing

-- | Append a 'LineTo' the start point of 'Path'.
close :: Path -> Path
close path =
    case startPt path of
      Just p -> path +++ ClosePath p
      Nothing -> path

-- | Approximate curves as /n/ straight line segments.  That is
-- replace 'CurveTo' nodes with /n/ 'LineTo' nodes calculated using
-- 'bezier4'.
approx :: Double -> Path -> Path
approx n path =
    case path of
      Join a (CurveTo p2 p3 p4) ->
          let is = [0, (1.0/n) .. 1.0]
              l = case endPt a of
                    Just p1 -> map (CG.bezier4 p1 p2 p3 p4) is
                    Nothing -> []
          in a +++ line l
      _ -> path

-- | Rectangle, given as lower left and upper right points.
data Rect a = Rect {rect_ll :: Pt a
                   ,rect_ur :: Pt a}
              deriving (Eq,Show)

-- | Sum (join) of two 'Rect'.
rect_sum :: Ord a => Rect a -> Rect a -> Rect a
rect_sum (Rect a b) (Rect c d) = Rect (CG.pt_min a c) (CG.pt_max b d)

-- | Rectangle bounding a (simple) 'Path'.
path_rect :: Path -> Rect Double
path_rect path =
    case path of
      MoveTo p -> Rect p p
      LineTo p -> Rect p p
      CurveTo c0 c1 p0 -> Rect (foldl1 CG.pt_min [c0,c1,p0]) (foldl1 CG.pt_max [c0,c1,p0])
      Join p q -> path_rect p `rect_sum` path_rect q
      _ -> error "path_rect: illegal query"

-- | Translate 'Rect' to 'BBox'.
rect_to_bbox :: RealFrac a => Rect a -> BBox
rect_to_bbox (Rect (Pt x0 y0) (Pt x1 y1)) = BBox (floor x0) (floor y0) (ceiling x1) (ceiling y1)

path_to_bbox :: Path -> BBox
path_to_bbox = rect_to_bbox . path_rect

{--


import Graphics.PS.Transform

join :: Path -> Path -> Path
join a b = a +++ translate x y b
    where (Pt x y) = endPt a

link :: Path -> Path -> Path
link a b = a +++ LineTo (startPt b) +++ b

relMoveTo :: Path -> Pt -> Path
relMoveTo path (Pt dx dy) = path +++ MoveTo (Pt (x + dx) (y + dy))
    where (Pt x y) = endPt path

relLineTo :: Path -> Pt -> Path
relLineTo path (Pt dx dy) = path +++ LineTo (Pt (x + dx) (y + dy))
    where (Pt x y) = endPt path

hasText :: Path -> Bool
hasText (Join p1 p2) = hasText p1 || hasText p2
hasText (Text _ _) = True
hasText _ = False

--}

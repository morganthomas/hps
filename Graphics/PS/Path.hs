-- | Path type and functions.
module Graphics.PS.Path where

import Data.List {- base -}
import Data.Monoid (Monoid, mappend, mconcat, mempty) {- base (obsolete) -}

import Data.CG.Minus {- hcg-minus -}

import Graphics.PS.Glyph
import Graphics.PS.Font

-- | Path data type,in cartesian space.
data Path = MoveTo (Pt Double)
          | LineTo (Pt Double)
          | CurveTo (Pt Double) (Pt Double) (Pt Double)
          | ClosePath (Pt Double)
          | Text Font [Glyph]
          | PTransform (Matrix Double) Path
          | Join Path Path
            deriving (Eq,Show)

instance Monoid Path where
    mempty = MoveTo (Pt 0 0)
    mappend = Join
    mconcat [] = mempty
    mconcat paths = combine paths

instance Semigroup Path where
    (<>) = Join

-- | Infix notation for 'Join'.
(+++) :: Path -> Path -> Path
(+++) = Join

-- | Left fold of 'Join'.
combine :: [Path] -> Path
combine = foldl1 Join

-- | Line segments though list of 'Pt'.
line :: [Pt Double] -> Path
line x =
    case x of
      [] -> error "line: illegal data"
      (p:ps) -> combine (MoveTo p : map LineTo ps)

-- | Variant of 'line' connecting the last 'Pt' to the first.
polygon :: [Pt Double] -> Path
polygon x =
    case x of
      [] -> error "polygon: illegal data"
      (p:ps) -> line (p:ps) +++ ClosePath p

-- | Rectangle with lower left at 'Pt' and of specified width and
-- height.  Polygon is ordered anticlockwise from lower left.
rectangle :: Pt Double -> Double -> Double -> Path
rectangle (Pt x y) w h =
    let ll = Pt x y
        lr = Pt (x + w) y
        ur = Pt (x + w) (y + h)
        ul = Pt x (y + h)
    in polygon [ll,lr,ur,ul]

-- | An arc segment, as starting point and values for the curveto operator.
type Arc_Seg n = (Pt n,Pt n,Pt n,Pt n)

-- | An arc, given as either one or two segments.
data Arc n = Arc1 (Arc_Seg n)
           | Arc2 (Arc_Seg n) (Arc_Seg n)

-- | Arc segment, (x,y) = center,r = radius,a = start angle,b = end angle.
arcp :: Pt Double -> Double -> Double -> Double -> Arc_Seg Double
arcp (Pt x y) r a b =
    let ca = cos a
        sa = sin a
        cb = cos b
        sb = sin b
        bcp = 4 / 3 * (1 - cos ((b - a) / 2)) / sin ((b - a) / 2)
        p0 = Pt (x + r * ca) (y + r * sa)
        p1 = Pt (x + r * (ca - bcp * sa)) (y + r * (sa + bcp * ca))
        p2 = Pt (x + r * (cb + bcp * sb)) (y + r * (sb - bcp * cb))
        p3 = Pt (x + r * cb) (y + r * sb)
    in (p0,p1,p2,p3)

-- | Arc, c = center,r = radius,a = start angle,b = end angle
--
-- If the arc angle is greater than pi the arc must be drawn in two segments.
arca :: Pt Double -> Double -> Double -> Double -> Arc Double
arca c r a b =
    let d = abs (b - a)
        b' = b - (d / 2)
    in if d > pi
       then Arc2 (arcp c r a b') (arcp c r b' b)
       else Arc1 (arcp c r a b)

-- | 'Path' of 'Arc'.
arc_to_path :: Arc Double -> Path
arc_to_path a =
    case a of
      Arc1 (p0,p1,p2,p3) -> MoveTo p0 +++ CurveTo p1 p2 p3
      Arc2 (p0,p1,p2,p3) (_,p5,p6,p7) -> MoveTo p0 +++ CurveTo p1 p2 p3 +++ CurveTo p5 p6 p7

-- | Variant of 'arca' allowing b to be less than a.
arca_udir :: Pt Double -> Double -> Double -> Double -> Arc Double
arca_udir c r a b =
    let b' = if b < a then b + 2 * pi else b
    in arca c r a b'

-- | 'arca_udir' with a and b reversed.
arcNegative_udir :: Pt Double -> Double -> Double -> Double -> Arc Double
arcNegative_udir c r a b =
    let b' = if b > a then b - 2 * pi else b
    in arca c r b' a

-- | Arc given by a central point,a radius,and start and end angles.
arc :: Pt Double -> Double -> Double -> Double -> Path
arc c r a = arc_to_path . arca_udir c r a

-- | Negative arc.
arcNegative :: Pt Double -> Double -> Double -> Double -> Path
arcNegative c r a = arc_to_path . arcNegative_udir c r a

type Annular n = (Pt n,Pt n,Arc n,Pt n,Arc n)

-- | (x,y) = center,ir = inner radius,xr = outer radius,sa = start
-- angle,a = angle,ea = end angle
annular_f :: Pt Double -> Double -> Double -> Double -> Double -> Annular Double
annular_f (Pt x y) ir xr sa a =
    let ea = sa + a
        x2 = x + ir * cos sa -- ll
        y2 = y + ir * sin sa
        x3 = x + xr * cos sa -- ul
        y3 = y + xr * sin sa
        x4 = x + ir * cos ea -- lr
        y4 = y + ir * sin ea
    in (Pt x2 y2
       ,Pt x3 y3
       ,arca_udir (Pt x y) xr sa ea
       ,Pt x4 y4
       ,arcNegative_udir (Pt x y) ir ea sa)

-- | Annular segment.
annular :: Pt Double -> Double -> Double -> Double -> Double -> Path
annular c ir xr sa a =
    let (p1,p2,a1,p3,a2) = annular_f c ir xr sa a
    in combine [MoveTo p1,LineTo p2,arc_to_path a1,LineTo p3,arc_to_path a2]

flatten_f :: Matrix Double -> Path -> Path
flatten_f m path =
    case path of
      MoveTo p -> MoveTo (pt_transform m p)
      LineTo p -> LineTo (pt_transform m p)
      ClosePath p -> ClosePath (pt_transform m p)
      PTransform m' p -> flatten_f (m' * m) p
      Join a b -> Join (flatten_f m a) (flatten_f m b)
      CurveTo p q r -> let f = pt_transform m
                       in CurveTo (f p) (f q) (f r)
      Text _ _ -> error "cannot flatten text"

-- | Apply any transformations at path.  The resulting path will not
--   have any 'PTransform' nodes.
flatten :: Path -> Path
flatten = flatten_f mx_identity

-- | Render each (p1,p2) as a distinct line.
renderLines :: [Ln Double] -> Path
renderLines =
    let f pth (Ln p1 p2) = pth +++ MoveTo p1 +++ LineTo p2
    in foldl f (MoveTo pt_origin)

-- | Collapse line sequences into a single line.
renderLines_jn :: [Ln Double] -> Path
renderLines_jn =
    let g p (Ln a b) = if p == a
                       then (b,Right b)
                       else (b,Left (Ln a b))
        f path e = case e of
                     Left (Ln p1 p2) -> path +++ MoveTo p1 +++ LineTo p2
                     Right p2 -> path +++ LineTo p2
    in foldl f (MoveTo pt_origin) . snd . mapAccumL g pt_origin

{--

curve :: Pt -> Pt -> Pt -> Pt -> Path
curve p c1 c2 q = MoveTo p +++ CurveTo c1 c2 q

-- | Polar variant.
pMoveTo :: Pt -> Path
pMoveTo p = MoveTo (polarToRectangular p)

-- | Polar variant.
pLineTo :: Pt -> Path
pLineTo p = LineTo (polarToRectangular p)

-- | Apply a funtion to leaf nodes.
p_apply :: (Path -> Path) -> Path -> Path
p_apply f (Join p q) = Join (f p) (f q)
p_apply f (PTransform m p) = PTransform m (f p)
p_apply f p = f p

--}

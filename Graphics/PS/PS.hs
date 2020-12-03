-- | Postscript generator.
module Graphics.PS.PS where

import Data.CG.Minus {- hcg-minus -}
import Data.List {- base -}
import Data.Monoid {- base -}
import System.IO {- base -}

import Graphics.PS.Path
import Graphics.PS.Font
import Graphics.PS.GS
import qualified Graphics.PS.Paper as P
import qualified Graphics.PS.Image as I

data PS = Name String
        | LName String
        | Op String
        | Comment String
        | Int Int
        | Double Double
        | String String
        | Transform (Matrix Double)
        | Array [PS]
        | Proc [PS]
        | Dict [(PS,PS)]
        | Seq [PS]

dscPS :: String -> [String] -> PS
dscPS k v =
    case v of
      [] -> Comment ('%' : k)
      _ ->Comment (concat ("%" : k : ": " : intersperse " " v))

headerPS :: PS
headerPS = Comment "!PS-Adobe-3.0"

headerEPS :: PS
headerEPS = Comment "!PS-Adobe-3.0 EPSF-3.0"

titlePS :: String -> PS
titlePS t = dscPS "Title" [t]

creatorPS :: String -> PS
creatorPS t = dscPS "Creator" [t]

languageLevelPS :: Int -> PS
languageLevelPS n = dscPS "LanguageLevel" [show n]

pagesPS :: Int -> PS
pagesPS n = dscPS "Pages" [show n]

bboxPS :: P.BBox -> PS
bboxPS b =
    case b of
      P.BBox i j k l -> dscPS "BoundingBox" [show i,show j,show k,show l]
      P.HRBBox i j k l i' j' k' l' ->
          Seq [dscPS "BoundingBox" [show i,show j,show k,show l],
               dscPS "HiResBoundingBox" [show i',show j',show k',show l']]

endCommentsPS :: PS
endCommentsPS = dscPS "EndComments" []

pagePS :: (Show a) => String -> a -> PS
pagePS t n = dscPS "Page" [t, show n]

trailerPS :: PS
trailerPS = dscPS "Trailer" []

eofPS :: PS
eofPS = dscPS "EOF" []

documentMediaPS :: String -> Int -> Int -> PS
documentMediaPS tag w h = dscPS "DocumentMedia" [tag, show w, show h, "0", "()", "()"]

aliasPS :: String -> String -> PS
aliasPS o a = Seq [LName a, Proc [Name o], Op "def"]

pdfCompat :: [(String, String)]
pdfCompat = [("gsave",                "q"),
             ("grestore",             "Q"),
             ("stroke",               "S"),
             ("fill",                 "f"),
             ("setrgbcolor",          "RG"),
             ("setlinewidth",         "w"),
             ("setlinecap",           "J"),
             ("setlinejoin",          "j"),
             ("setdash",              "d"),
             ("setmiterlimit",        "M"),
             ("moveto",               "m"),
             ("lineto",               "l"),
             ("curveto",              "c"),
             ("closepath",            "h"),
             ("concat",               "cm"),
             ("show",                 "Tj"),
             ("selectfont",           "Tf"),
             ("clip",                 "W")]

prologPS :: PS
prologPS =
    let f (a,b) = aliasPS a b
    in Seq (map f pdfCompat)

strokePS :: PS
strokePS = Op "S"

fillPS :: PS
fillPS = Op "f"

falsePS :: PS
falsePS = Name "false"

savePS :: PS
savePS = Op "q"

restorePS :: PS
restorePS = Op "Q"

showPagePS :: PS
showPagePS = Op "showpage"

rgbaPS :: Color -> PS
rgbaPS (RGBA r g b _) = Seq [Double r, Double g, Double b, Op "RG"]

lineWidthPS :: Double -> PS
lineWidthPS w = Seq [Double w, Op "w"]

lineCapPS :: (Enum a) => a -> PS
lineCapPS c = Seq [Int (fromEnum c), Op "j"]

lineJoinPS :: (Enum a) => a -> PS
lineJoinPS j = Seq [Int (fromEnum j), Op "J"]

dashPS :: [Int] -> Int -> PS
dashPS d o = Seq [Array (map Int d), Int o, Op "d"]

miterLimitPS :: Double -> PS
miterLimitPS m = Seq [Double m, Op "M"]

moveToPS :: Pt Double -> PS
moveToPS (Pt x y) = Seq [Double x, Double y, Op "m"]

lineToPS :: Pt Double -> PS
lineToPS (Pt x y) = Seq [Double x, Double y, Op "l"]

transformPS :: Matrix Double -> PS
transformPS m = Seq [Transform m, Op "cm"]

curveToPS :: Pt Double -> Pt Double -> Pt Double -> PS
curveToPS a b c = Seq (map Double (ls_xy (Ls [a,b,c])) ++ [Op "c"])

closePathPS :: Pt Double -> PS
closePathPS (Pt x y) = Seq [Double x, Double y, Op "h"]

selectFontPS :: Font -> PS
selectFontPS (Font f n) = Seq [LName f, Double n, Op "Tf"]

charPathPS :: String -> PS
charPathPS g = Seq [String g, falsePS, Op "charpath"]

gsPS :: GS -> PS
gsPS (GS c w k j (d, o) m) =
    Seq [rgbaPS c
        ,lineWidthPS w
        ,lineCapPS k
        ,lineJoinPS j
        ,dashPS d o
        ,miterLimitPS m]

pathPS :: Path -> PS
pathPS path =
    case path of
      MoveTo p -> moveToPS p
      LineTo p -> lineToPS p
      Text f g -> Seq [selectFontPS f, charPathPS g]
      CurveTo c1 c2 p -> curveToPS c1 c2 p
      ClosePath p -> closePathPS p
      PTransform m p -> Seq [transformPS m, pathPS p]
      Join a b -> Seq [pathPS a, pathPS b]

imagePS :: I.Image -> PS
imagePS img =
    case img of
      I.Empty -> Comment "Empty"
      I.Stroke g p -> Seq [pathPS p, gsPS g, strokePS]
      I.Fill g p -> Seq [pathPS p, gsPS g, fillPS]
      I.ITransform m i -> Seq [transformPS m, imagePS i]
      I.Over a b -> Seq [savePS
                        ,imagePS b
                        ,restorePS
                        ,imagePS a]

infixl 1 >+>

(>+>) :: Monoid m => m -> m -> m
(>+>) = mappend

ps_bracket :: (Monoid m) => (String -> m) -> String -> String -> [a] -> (a -> m) -> m
ps_bracket f o c p g =
    let h a = f a >+> f " "
    in h o >+> mconcat (map g p) >+> h c

ps_escape :: String -> String
ps_escape = concatMap (\c -> if elem c "()" then ['\\', c] else [c])

ps_put :: (Monoid m) => (String -> m) -> PS -> m
ps_put f x =
    case x of
      Name n -> f n >+> f " "
      LName n -> f "/" >+> f n >+> f " "
      Op o -> f o >+> f "\n"
      Comment o -> f "%" >+> f o >+> f "\n"
      Int n -> f (show n) >+> f " "
      Double n -> f (show n) >+> f " "
      String s -> f "(" >+> f (ps_escape s) >+> f ") "
      Array a -> ps_bracket f "[" "]" a (ps_put f)
      Proc p -> ps_bracket f "{" "}" p (ps_put f)
      Transform m -> ps_put f (Array (map Double (mx_list m)))
      Dict d -> let g = concatMap (\(a,b) -> [a,b]) in ps_bracket f "<<" ">>" (g d) (ps_put f)
      Seq a -> mconcat (map (ps_put f) a)

to_page_seq :: (I.Image, Int) -> PS
to_page_seq (p, n) = Seq [pagePS "Graphics.PS" n, imagePS p, showPagePS]

-- | Write a postscript file.  The list of images are written one per page.
ps :: FilePath -> P.Paper -> [I.Image] -> IO ()
ps f d p = writeFile f (stringFromPS f d p)

-- | Variant with page (paper) size in points.
stringFromPS_pt :: String -> (Int,Int) -> [I.Image] -> String
stringFromPS_pt t (width,height) p =
  let g = ps_put (\s -> Endo (s++))
  in flip appEndo "" $
     g headerPS >+>
     g (titlePS t) >+>
     g (creatorPS "Graphics.PS") >+>
     g (languageLevelPS 2) >+>
     g (pagesPS (length p)) >+>
     g (documentMediaPS "Default" width height) >+>
     g endCommentsPS >+>
     g prologPS >+>
     mconcat (map (g . to_page_seq) (zip p [1..])) >+>
     g trailerPS >+>
     g eofPS

-- | Generate postscript data given /title/, page size, and a
-- set of page 'I.Images'.
stringFromPS :: String -> P.Paper -> [I.Image] -> String
stringFromPS t p = stringFromPS_pt t (P.paper_size_pt p)

newtype MonadMonoid m = MonadMonoid {appMonadMonoid :: m ()}

instance Monad m => Monoid (MonadMonoid m) where
   mempty = MonadMonoid (return ())
   mappend (MonadMonoid a) (MonadMonoid b) =
      MonadMonoid (a >> b)
   mconcat = MonadMonoid . mapM_ appMonadMonoid

instance Monad m => Semigroup (MonadMonoid m) where
  MonadMonoid a <> MonadMonoid b = MonadMonoid (a >> b)

-- | Write an encapsulated postscript file.  The 'P.BBox' is in
-- points.  The single image is written.
eps :: FilePath -> P.BBox -> I.Image -> IO ()
eps fn d p =
  withFile fn WriteMode $ \h ->
     let g = ps_put (MonadMonoid . hPutStr h)
     in mapM_ (appMonadMonoid . g) $
            [headerEPS
            ,titlePS ("Graphics.PS: " ++ fn)
            ,creatorPS "Graphics.PS"
            ,languageLevelPS 2
            ,bboxPS d
            ,endCommentsPS
            ,prologPS
            ,imagePS p]

{--

data Orientation = Portrait
                 | Landscape
                   deriving (Show)

orientation :: Orientation -> PS
orientation o = dsc "Orientation" [show o]

creationDate :: String -> PS
creationDate t = dsc "CreationDate" [t]

setFont :: PS
setFont = Op "setfont"

show :: String -> PS
show g = [String g, Op "h"]

translate :: Double -> Double -> PS
translate x y = [Double x, Double y, Op "translate"]

scale :: Double -> Double -> PS
scale x y = [Double x, Double y, Op "scale"]

rotate :: Double -> PS
rotate a = [Double a, Op "rotate"]

findFont :: Font -> PS
findFont (Font f _) = [LName f, Op "findfont"]

scaleFont :: Font -> PS
scaleFont (Font _ n) = [Double n, Op "scalefont"]

--}

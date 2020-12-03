-- | PS graphics state.
module Graphics.PS.GS where

-- | Line cap enumeration.
data LineCap = ButtCap
             | RoundCap
             | ProjectingSquareCap
               deriving (Eq, Show, Enum)

-- | Line width (real).
type LineWidth = Double

-- | Line join enumeration.
data LineJoin = MiterJoin
              | RoundJoin
              | BevelJoin
                deriving (Eq, Show, Enum)

-- | Colour model.  Postscript doesn't support alpha, but store it for PDF rendering etc.
data Color = RGBA Double Double Double Double
             deriving (Eq, Show)

-- | Graphics state.
data GS = GS {gs_color :: Color
             ,gs_line_width :: LineWidth
             ,gs_line_cap :: LineCap
             ,gs_line_join :: LineJoin
             ,gs_dash :: ([Int], Int)
             ,gs_miter_limit :: Double}
          deriving (Eq, Show)

-- | Default 'GS' of indicated 'Color'.
defaultGS :: Color -> GS
defaultGS c = GS c 1.0 ButtCap MiterJoin ([], 0) 10.0

-- | Default 'GS' of indicated shade of grey.
greyGS :: Double -> GS
greyGS g = defaultGS (RGBA g g g 1)

{--
blackGS :: GS
blackGS = greyGS 0
--}

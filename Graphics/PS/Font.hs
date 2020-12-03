-- | Font type and functions.
module Graphics.PS.Font where

-- | Font data type.
data Font = Font {fontName :: String
                 ,fontSize :: Double}
            deriving (Eq,Show)

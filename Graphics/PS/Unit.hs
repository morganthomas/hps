-- | Unit definitions and conversions.
module Graphics.PS.Unit where

-- | Convert degrees to radians.
radians :: Floating a => a -> a
radians n = n / (360 / (2 * pi))

-- | Convert millimeters to PS points.
mm_pt :: Floating a => a -> a
mm_pt = (* 2.8346456664)

-- | Type specialised 'fromIntegral'.
i_to_d :: Int -> Double
i_to_d = fromIntegral

-- | 'Int' variant of 'mm_pt'.
mm_pt_int :: Int -> Int
mm_pt_int = floor . mm_pt . i_to_d

{--
-- | Convert PS points to millimeters.
pt_mm :: Floating a => a -> a
pt_mm = (/ 2.8346456664)

-- | Postscript makes a point exactly 1/72 of an inch.
inch :: (Num a) => a -> a
inch n = 72 * n

-- | 1 cm = 0.393700787 inches
cm :: Floating a => a -> a
cm n = 28.346456664 * n

twopi :: Floating a => a
twopi = 2 * pi

halfpi :: Floating a => a
halfpi = pi / 2

degrees :: Floating a => a -> a
degrees n = n * (360 / twopi)
--}

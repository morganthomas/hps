-- | Names of paper sizes.
-- For ISO sizes see <http://www.cl.cam.ac.uk/~mgk25/iso-paper.html>.
module Graphics.PS.Paper.Names where

import Graphics.PS.Paper

-- | ISO size downscaling, ie. from @A0@ to @A1@.
--
-- > iso_down_scale a4 == a5
iso_down_scale :: Paper -> Paper
iso_down_scale (Paper u w h) = Paper u (h `divRound` 2) w

-- | JIS size downscaling, truncates instead of rounding.
jis_down_scale :: Paper -> Paper
jis_down_scale (Paper u w h) = Paper u (h `div` 2) w

-- | ISO A sizes in millimeters.
--
-- > a4 == Paper 210 297
a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10 :: Paper
a0 = Paper MM 841 1189
a1 = iso_down_scale a0
a2 = iso_down_scale a1
a3 = iso_down_scale a2
a4 = iso_down_scale a3
a5 = iso_down_scale a4
a6 = iso_down_scale a5
a7 = iso_down_scale a6
a8 = iso_down_scale a7
a9 = iso_down_scale a8
a10 = iso_down_scale a9

-- | ISO B sizes in millimeters.
--
-- > b4 == Paper 250 354
-- > inset_margin b4 a3 == (23,33)
b0,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10 :: Paper
b0 = Paper MM 1000 1414
b1 = iso_down_scale b0
b2 = iso_down_scale b1
b3 = iso_down_scale b2
b4 = iso_down_scale b3
b5 = iso_down_scale b4
b6 = iso_down_scale b5
b7 = iso_down_scale b6
b8 = iso_down_scale b7
b9 = iso_down_scale b8
b10 = iso_down_scale b9

-- | JIS B sizes
--
-- > jis_b4 == Paper 257 364
-- > (proportion b0,proportion jis_b0) == (1.414,1.4135922330097088)
--
-- > inset_margin jis_b4 a3 == (20,28)
jis_b0,jis_b1,jis_b2,jis_b3,jis_b4,jis_b5,jis_b6,jis_b7,jis_b8,jis_b9,jis_b10 :: Paper
jis_b0 = Paper MM 1030 1456
jis_b1 = jis_down_scale jis_b0
jis_b2 = jis_down_scale jis_b1
jis_b3 = jis_down_scale jis_b2
jis_b4 = jis_down_scale jis_b3
jis_b5 = jis_down_scale jis_b4
jis_b6 = jis_down_scale jis_b5
jis_b7 = jis_down_scale jis_b6
jis_b8 = jis_down_scale jis_b7
jis_b9 = jis_down_scale jis_b8
jis_b10 = jis_down_scale jis_b9

-- | ISO C sizes in millimeters.
--
-- > c4 == Paper 229 324
c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10 :: Paper
c0 = Paper MM 917 1297
c1 = iso_down_scale c0
c2 = iso_down_scale c1
c3 = iso_down_scale c2
c4 = iso_down_scale c3
c5 = iso_down_scale c4
c6 = iso_down_scale c5
c7 = iso_down_scale c6
c8 = iso_down_scale c7
c9 = iso_down_scale c8
c10 = iso_down_scale c9

-- | US Letter size in millimeters (ie 'Paper' @216 279@).
usLetter :: Paper
usLetter = Paper MM 216 279

-- | Newspaper sizes in millimeters.
-- See <http://www.papersizes.org/newspaper-sizes.htm>.
broadsheet,berliner,tabloid :: Paper
broadsheet = Paper MM 600 750
berliner = Paper MM 315 470
tabloid = Paper MM 280 430

{-# Language FlexibleInstances #-}
-- | Class and associated functions for 'Matrix' transformations.
module Graphics.PS.Transform where

import qualified Data.CG.Minus as CG {- hcg-minus -}

import qualified Graphics.PS.Path as P
import qualified Graphics.PS.Image as I

-- | Values that can be transformed in relation to a 'Matrix'.
class Transformable t where
    mtransform :: CG.Matrix Double -> t -> t

-- | Translation in /x/ and /y/.
translate :: Transformable t => Double -> Double -> t -> t
translate x = mtransform . CG.mx_translation x

-- | Scaling in /x/ and /y/.
scale :: Transformable t => Double -> Double -> t -> t
scale x = mtransform . CG.mx_scaling x

-- | Rotation, in radians.
rotate :: Transformable t => Double -> t -> t
rotate = mtransform . CG.mx_rotation

instance Transformable I.Image where
    mtransform = I.ITransform

instance Transformable P.Path where
    mtransform = P.PTransform

instance Transformable (CG.Pt Double) where
    mtransform = CG.pt_transform

{--
import Graphics.PS.Pt

-- | Polar variant.
pTranslate :: (Transform a) => Double -> Double -> a -> a
pTranslate r t = translate x y
    where (Pt x y) = polarToRectangular (Pt r t)

--}

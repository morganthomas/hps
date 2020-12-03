-- | Image type and functions.
module Graphics.PS.Image where

import Data.Monoid (Monoid, mappend, mconcat, mempty) {- base (obsolete) -}

import Data.CG.Minus.Types {- hcg-minus -}

import Graphics.PS.Path
import Graphics.PS.GS

-- | An image is a rendering of a graph of 'Path's.
data Image = Stroke GS Path
           | Fill GS Path
           | ITransform (Matrix Double) Image
           | Over Image Image
           | Empty
             deriving (Eq, Show)

instance Monoid Image where
    mempty = Empty
    mappend = Over
    mconcat = foldr Over Empty

instance Semigroup Image where
    (<>) = Over

-- | Layer one 'Image' over another.
over :: Image -> Image -> Image
over = Over

-- | List of 'Path's at 'Image'.
paths :: Image -> [Path]
paths =
    let rec m i =
            let f = maybe id PTransform m
            in case i of
                 Stroke _ p -> [f p]
                 Fill _ p -> [f p]
                 ITransform m' i' ->
                     let m'' = maybe m' (* m') m
                     in rec (Just m'') i'
                 Over l r -> rec m l ++ rec m r
                 Empty -> []
    in rec Nothing

{-
-- | Apply a function to leaf nodes.
i_apply :: (Image -> Image) -> Image -> Image
i_apply f (ITransform m i) = ITransform m (i_apply f i)
i_apply f (Over i j) = Over (i_apply f i) (i_apply f j)
i_apply f i = f i

-- | Apply a path function to leaf nodes.
i_p_apply :: (P.Path -> P.Path) -> Image -> Image
i_p_apply f (Stroke g p) = Stroke g (P.p_apply f p)
i_p_apply f (Fill g p) = Fill g (P.p_apply f p)
i_p_apply f (ITransform m i) = ITransform m (i_p_apply f i)
i_p_apply f (Over i j) = Over (i_p_apply f i) (i_p_apply f j)
i_p_apply _ Empty = Empty
-}

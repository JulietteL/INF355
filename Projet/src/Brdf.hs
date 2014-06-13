module Brdf where

import Data.Maybe
import Graphics.GD
import Scene
import Ray
import Vector

-- Old version
-- brdf :: [Object] -> Ray -> Color
-- brdf objs (Ray o d) = sum( fmap (\r -> if isJust r then rgb 255 255 255 else rgb 0 0 0 )[intersect (Ray o d) obj | obj <- objs ])
  

brdf :: [Object] -> [Light] -> Ray -> Color
brdf objs lights (Ray o d) = sum( fmap (\(r, mat) -> if isJust r then
      let (p,n) = fromJust(r)
      in toColor ( brdf' (p, n, mat) lights o)
                                                     else rgb 0 0 0 )[(intersect (Ray o d) obj, getMaterial(obj)) | obj <- objs ])

-- brdf : (pt intersection, normale au pt d'intersection, matériau), lumières, position de la caméra -> Couleur 
brdf' :: (Vec3Df, Vec3Df, Material) -> [Light] -> Vec3Df -> Vec3Df
brdf' (p, n, mat) lights cam = sum $ fmap (\(Light lp lc) -> mul (dot (normalize (lp-p)) n) ( lc * getDiffuseColor mat)) lights

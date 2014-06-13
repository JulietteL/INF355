module Brdf where

import Data.List hiding (intersect)
import Data.Ord
import Data.Maybe
import Graphics.GD
import Scene
import Ray
import Vector

-- brdf : (pt intersection, normale au pt d'intersection, matériau), lumières, position de la caméra -> Couleur 
brdf' :: ((Vec3Df, Vec3Df), Material) -> [Light] -> Vec3Df -> Vec3Df
brdf' ((p, n), mat) lights cam = sum $ fmap (\(Light lp lc) -> mul (dot (normalize (lp-p)) n) ( lc * getDiffuseColor mat)) lights

sqDistanceToCam :: Vec3Df -> (Maybe (Vec3Df,Vec3Df), Material) -> Float
sqDistanceToCam cam (Just (point,normal), _) = squaredNorm $ point - cam
                      
brdf :: [Object] -> [Light] -> Ray -> Color
brdf objs lights (Ray o d) = let intList = filter (\x -> isJust $ fst x) [(intersect (Ray o d) obj, getMaterial(obj)) | obj <- objs ]
                      in if null intList
                         then rgb 0 0 0
                         else toColor $
                              brdf' ((\r -> (fromJust $ fst r, snd r)) (minimumBy (comparing (sqDistanceToCam o)) intList)) lights o

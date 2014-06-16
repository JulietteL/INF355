module Brdf where

import Data.List hiding (intersect)
import Data.Ord
import Data.Maybe
import Graphics.GD
import Scene
import Ray
import Vector

bgColor :: Color
bgColor = rgb 10 10 10

sqDistanceToCam :: Vec3Df -> (Maybe (Vec3Df,Vec3Df), Material) -> Float
sqDistanceToCam cam (Just (point,normal), _) = squaredNorm $ point - cam
                      
brdf :: [Object] -> [Light] -> Ray -> String -> Color
brdf objs lights (Ray o d) brdfName = let intList = filter (\x -> isJust $ fst x) [(intersect (Ray o d) obj, getMaterial(obj)) | obj <- objs ]
                      in if null intList
                         then bgColor
                         else toColor $
                              brdf' ((\r -> (fromJust $ fst r, snd r)) (minimumBy (comparing (sqDistanceToCam o)) intList)) lights o brdfName

-- brdf : (pt intersection, normale au pt d'intersection, matériau), lumières, position de la caméra -> Couleur 
brdf' :: ((Vec3Df, Vec3Df), Material) -> [Light] -> Vec3Df -> String -> Vec3Df
-- Lambert
brdf' ((p, n), mat) lights _ "lambert"= sum $ fmap (\(Light lp lc) -> mul (dot (normalize (lp-p)) n) ( lc * getDiffuseColor mat)) lights
-- Phong
brdf' ((p, n), (SpecularMaterial d kd s ks sh)) lights cam "phong"=
  sum $ fmap (\(Light lp lc) -> let  h = normalize $ (lp - p) + (cam - p)
                                in ((mul (kd * dot (normalize $ lp-p) n) d)
                                    + mul (ks * (dot h n) ** sh) s) * lc
             )
  lights
brdf' _ _ _ "phong" = error "object's material is not of type SpecularMaterial"
brdf' _ _ _ _ = error "no such BRDF"

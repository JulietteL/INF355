module Brdf where

import Data.List hiding (intersect)
import Data.Ord
import Data.Maybe
import Graphics.GD
import Scene
import Ray
import Vector

bgColor :: Vec3Df
bgColor = (Vec3Df 10 10 10)

sqDistanceToCam :: Vec3Df -> (Maybe (Vec3Df,Vec3Df), Material) -> Float
sqDistanceToCam cam (Just (point,normal), _) = squaredNorm $ point - cam
                      
brdf :: [Object] -> [Light] -> Ray -> String -> Int -> Vec3Df
brdf objs lights (Ray o d) brdfName i = let intList = filter (\x -> isJust $ fst x) [(intersect (Ray o d) obj, getMaterial(obj)) | obj <- objs ]
                      in if null intList
                         then bgColor
                         else let
                           ((p, n), mat) = ((\r -> (fromJust $ fst r, snd r)) (minimumBy (comparing (sqDistanceToCam o)) intList))
                           brdf1 = brdf' ((p,n), mat) lights o brdfName
                              in if getReflexiveCoeff mat == 0
                                 then brdf1 
                                 else if i < 5
                                      then mul (getReflexiveCoeff mat) (brdf objs lights (reflectedRay (Ray o d) (p,n)) brdfName (i+1)) + mul (1-getReflexiveCoeff mat) brdf1
                                      else brdf1
                                           
-- brdf : (pt intersection, normale au pt d'intersection, matériau), lumières, position de la caméra -> Couleur 
brdf' :: ((Vec3Df, Vec3Df), Material) -> [Light] -> Vec3Df -> String -> Vec3Df
-- Lambert
brdf' ((p, n), mat) lights _ "lambert"= sum $ fmap (\(Light lp lc) -> mul (max 0 $ dot (normalize (lp-p)) n) ( lc * getDiffuseColor mat)) lights
-- Phong
brdf' ((p, n), (SpecularMaterial d kd s ks sh)) lights cam "phong"=
  sum $ fmap (\(Light lp lc) -> let  h = normalize $ (normalize (lp - p)) + (normalize (cam - p))
                                in ((mul (kd * (max 0 $ dot (normalize $ lp-p) n)) d)
                                    + mul (ks * (max 0 $ dot h n) ** sh) s) * lc
             )
  lights
  
-- Reflexive
brdf' ((p, n), (ReflexiveMaterial f d kd s ks sh)) lights cam "phong" =
  brdf' ((p, n), (SpecularMaterial d kd s ks sh)) lights cam "phong"
  
brdf' _ _ _ "phong" = error "object's material is not of type SpecularMaterial"
brdf' _ _ _ _ = error "no such BRDF"

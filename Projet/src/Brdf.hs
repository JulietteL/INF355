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
                           brdf1 = brdf' ((p,n), mat) lights o brdfName objs
                              in if getReflexiveCoeff mat == 0
                                 then brdf1 
                                 else if i < 5
                                      then mul (getReflexiveCoeff mat) (brdf objs lights (reflectedRay (Ray o d) (p,n)) brdfName (i+1)) + mul (1-getReflexiveCoeff mat) brdf1
                                      else brdf1
                                           
shadowCoeff :: Vec3Df -> Light -> [Object] -> Float
shadowCoeff pos (Light lp lc) objs = let dir = normalize $ lp - pos
                                     in let ray = Ray (pos +  (mul 0.01 dir)) dir
                                      in let intList = filter (\x -> isJust x && squaredNorm (fst (fromJust x) - pos) < squaredNorm (lp - pos)) [intersect ray obj | obj <- objs]
                                             
                                         in if null intList
                                            then 1.0
                                            else 0.0
                                                 
-- brdf : (pt intersection, normale au pt d'intersection, matériau), lumières, position de la caméra -> Couleur 
brdf' :: ((Vec3Df, Vec3Df), Material) -> [Light] -> Vec3Df -> String -> [Object] -> [Float] -> (Vec3Df, [Float])
-- Lambert
brdf' ((p, n), mat) lights _ "lambert" _ random = (sum $ fmap (\(Light lp lc) -> mul (max 0 $ dot (normalize (lp-p)) n) ( lc * getDiffuseColor mat)) lights, random)
-- Phong
brdf' u light cam "phong" objs random  = brdf'' u light cam "phong" objs random
brdf' u (light:otherLights) cam "phong" objs random =
  let (c, random') = brdf'' u light cam "phong" objs random
      (c', random'') = brdf' u otherLights cam "phong" objs random'
  in (c + c', random'')

brdf'' :: ((Vec3Df, Vec3Df), Material) -> Light -> Vec3Df -> String -> [Object] -> [Float] -> (Vec3Df, [Float])
brdf'' ((p, n), (SpecularMaterial d kd s ks sh)) (Light lp lc) cam "phong" objs random =
  let h = normalize $ (normalize (lp - p)) + (normalize (cam - p))
      (lights', random') = getPointsOnLight (Light lp lc) 4 random
      sc = sum $ fmap (\x -> shadowCoeff p x objs) lights'
  in (mul sc $ ((mul (kd * (max 0 $ dot (normalize $ lp-p) n)) d)
               + mul (ks * (max 0 $ dot h n) ** sh) s) * lc, random')
            
-- Reflexive
brdf' ((p, n), (ReflexiveMaterial f d kd s ks sh)) lights cam "phong" objs =
  brdf' ((p, n), (SpecularMaterial d kd s ks sh)) lights cam "phong" objs
  
brdf' _ _ _ "phong"_ = error "object's material is not of type SpecularMaterial"
brdf' _ _ _ _ _ = error "no such BRDF"


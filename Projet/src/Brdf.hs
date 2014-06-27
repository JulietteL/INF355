module Brdf where

import Data.List hiding (intersect)
import Data.Ord
import Data.Maybe
import Graphics.GD
import Scene
import Ray
import Vector

-- global parameters

-- Background Color
bgColor :: Vec3Df
bgColor = (Vec3Df 10 10 10)

-- Activate shadows
shadows :: Bool
shadows = True

-- Number of rays casted to extended lights (soft shadow)
shadowRays :: Int
shadowRays = 16

-- Maximum number of rebounds for mirrors
maxRebounds :: Int
maxRebounds = 5
-------------------------

-- function used to sort the intersections 
sqDistanceToCam :: Vec3Df -> (Maybe (Vec3Df,Vec3Df), Material) -> Float
sqDistanceToCam cam (Just (point,_), _) = squaredNorm $ point - cam

-- Apply the ray tracing algorithm to a set of antialiasing rays
-- Parameters : scene objects, scene lights, rays, brdf type, list of random numbers
-- Returns : (brdf value, tail of the random numbers' list)
brdfs :: [Object] -> [Light] -> [Ray] -> String -> [Float] -> (Vec3Df, [Float])
brdfs _ _ [] _ rlist = (0, rlist)
brdfs objs lights (ray:t) brdfName rlist = let (c,rlist') = brdf objs lights ray brdfName 0 rlist
                                               (c',rlist'') = brdfs objs lights t brdfName rlist'
                                             in (c+c', rlist'') 

-- Apply the ray tracing algorithm to a single ray
-- Parameters : scene objects, scene lights, ray, brdf type, mirror iteration, list of random numbers
-- Returns : (brdf value, tail of the random numbers' list)
brdf :: [Object] -> [Light] -> Ray -> String -> Int -> [Float] -> (Vec3Df, [Float])
brdf objs lights (Ray o d) brdfName i rList= let intList = filter (\x -> isJust $ fst x) [(intersect (Ray o d) obj, getMaterial(obj)) | obj <- objs ]
                      in if null intList
                         then (bgColor, rList)
                         else let
                           ((p, n), mat) = ((\r -> (fromJust $ fst r, snd r)) (minimumBy (comparing (sqDistanceToCam o)) intList))
                           (brdf1, rList') = brdf' ((p,n), mat) lights o brdfName objs rList
                              in if getReflexiveCoeff mat == 0
                                 then (brdf1, rList') 
                                 else if i < maxRebounds
                                      then let (brdf2, rList'') = brdf objs lights (reflectedRay (Ray o d) (p,n)) brdfName (i+1) rList'
                                           in (mul (getReflexiveCoeff mat) brdf2 + mul (1 - getReflexiveCoeff mat) brdf1, rList'')
                                      else (brdf1, rList')


-- Calculate the shadow coefficient on a point by casting a ray to the light
-- Parameters : position, light, scene objects
-- Return : 1 if a scene object is intersected, else 0
shadowCoeff :: Vec3Df -> Light -> [Object] -> Float
shadowCoeff pos (ExtendedLight lp lc _) objs = shadowCoeff pos (Light lp lc) objs
shadowCoeff pos (Light lp _) objs = let dir = normalize $ lp - pos
                                     in let ray = Ray (pos +  (mul 0.01 dir)) dir
                                      in let intList = filter (\x -> isJust x && squaredNorm (fst (fromJust x) - pos) < squaredNorm (lp - pos)) [intersect ray obj | obj <- objs]
                                             
                                         in if null intList
                                            then 1.0
                                            else 0.0

-- Apply the Bidirectional Reflectance Distribution Function at an intersected object
-- Parameters : intersectionPoint : ((position,normal), object material), scene lights, camera position, brdf type, scene objects, list of random numbers
-- Returns : (color, tail of the random numbers' list)
-- brdf types are "lambert" and "phong" 
brdf' :: ((Vec3Df, Vec3Df), Material) -> [Light] -> Vec3Df -> String -> [Object] -> [Float] -> (Vec3Df, [Float])
-- Lambert
brdf' ((p, n), mat) lights _ "lambert" _ random = (sum $ fmap (\(Light lp lc) -> mul (max 0 $ dot (normalize (lp-p)) n) ( lc * getDiffuseColor mat)) lights, random)
-- Phong
brdf' u [light] cam "phong" objs random  = brdf'' u light cam "phong" objs random
brdf' u (light:otherLights) cam "phong" objs random =
  let (c, random') = brdf'' u light cam "phong" objs random
      (c', random'') = brdf' u otherLights cam "phong" objs random'
  in (c + c', random'')            
  
brdf' _ _ _ "phong"_ _ = error "object's material is not of type SpecularMaterial"
brdf' _ _ _ _ _ _ = error "no such BRDF"


-- Apply the BRDF component of one light
-- Parameters : intersectionPoint ((position,normal), object material), light, camera position, brdf type, scene objects, list of random numbers
-- Returns : (color, tail of the random numbers' list)
brdf'' :: ((Vec3Df, Vec3Df), Material) -> Light -> Vec3Df -> String -> [Object] -> [Float] -> (Vec3Df, [Float])
brdf'' ((p, n), (SpecularMaterial d kd s ks sh)) (ExtendedLight lp lc r) cam "phong" objs random =
  let h = normalize $ (normalize (lp - p)) + (normalize (cam - p))
      (lights', random') = getPointsOnLight (ExtendedLight lp lc r) shadowRays random
      sc = if shadows
           then (sum $ fmap (\x -> shadowCoeff p x objs) lights')/(fromIntegral shadowRays)
           else 1
  in (mul sc $ ((mul (kd * (max 0 $ dot (normalize $ lp-p) n)) d)
               + mul (ks * (max 0 $ dot h n) ** sh) s) * lc, random')
brdf'' ((p, n), (SpecularMaterial d kd s ks sh)) (Light lp lc) cam "phong" objs random =
  let h = normalize $ (normalize (lp - p)) + (normalize (cam - p))
      sc = if shadows
           then shadowCoeff p (Light lp lc) objs
           else 1 
  in (mul sc $ ((mul (kd * (max 0 $ dot (normalize $ lp-p) n)) d)
               + mul (ks * (max 0 $ dot h n) ** sh) s) * lc, random)
-- reflexive
brdf'' ((p, n), (ReflexiveMaterial _ d kd s ks sh)) light cam "phong" objs random =
  brdf'' ((p, n), (SpecularMaterial d kd s ks sh)) light cam "phong" objs random


-- WARNING : debug used to find non handled cases
brdf'' _ _ _ _ _ rList = (Vec3Df 0.0 0.0 255.0, rList)

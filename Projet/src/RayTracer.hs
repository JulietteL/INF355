module RayTracer where

import System.Random
import Graphics.GD

import Vector
import Scene
import SceneExamples
import Ray
import Brdf

-- Compute useful tangents to calculate the steps between different rays directions
-- field of view angle : 80 degrees
tanX :: Float -> Float -> Float
tanX h w = tan (80 * pi/180) * w/h 
tanY :: Float
tanY = tan (80 * pi/180)

-- WARNING!! obsolete
-- rayTrace: randomList height width rightVector upVector antialiasingRays Scene imagePixel -> pixel color
rayTrace' :: Float -> Float -> Vec3Df -> Vec3Df -> Integer -> Scene -> Point -> Color
rayTrace' h w rv uv n ((Camera o t),objs, lights) (x,y) = let stepX = mul ((tanX w h)/h) rv
                                                              stepY = mul (tanY/w) uv
                                      in let dir = t - o + (mul (intToFloat x - h/2) stepX) + (mul (intToFloat y - w/2) stepY)
                                         in let dirs = antiAliasing n stepX stepY dir
                                            in toColor $ divl (intToFloat $ length dirs) (sum $ [fst $brdf objs lights (Ray o (normalize d)) "phong" 0 []| d <- dirs])

-- Compute the color value on one pixel using the ray tracing algorithm
-- Parameters : image height, image width, camera up vector, camera right vector, number of antialisating rays, scene, random numbers list, image point
-- Return : (color, tail of the random list)
rayTrace :: Float -> Float -> Vec3Df -> Vec3Df -> Integer -> Scene -> [Float] -> Point -> (Color, [Float])
rayTrace h w rv uv n ((Camera o t),objs, lights) randomList (x,y) = let stepX = mul ((tanX w h)/h) rv
                                                                        stepY = mul (tanY/w) uv
                                      in let dir = t - o + (mul (intToFloat x - h/2) stepX) + (mul (intToFloat y - w/2) stepY)
                                         in let (dirs,rlist) = randomAL randomList n stepX stepY dir
                                                rays = [(Ray o (normalize d)) | d <- dirs]
                                                (color,rlist') = brdfs objs lights rays "phong" rlist
                                            in (toColor $ divl (fromInteger n) color, rlist')

-- Get uniformly distributed antialiasing rays
-- antiAliasing: rayNumber stepX stepY rayDir -> rayDirections
antiAliasing :: Integer -> Vec3Df -> Vec3Df -> Vec3Df -> [Vec3Df]
antiAliasing n stepX stepY dir = let stepx = mul 0.5 stepX
                                     stepy = mul 0.5 stepY
                                 in
                                  case n of
                                    1 -> [dir]
                                    5 -> map (dir+) [0,stepx,-stepx,stepy,-stepy]
                                    9 -> map (dir+) [0,stepx,-stepx,stepy,-stepy,stepx+stepy,stepx-stepy,-stepx+stepy,-stepx-stepy]
                                    _ -> error "unsupported antialiasing parameter"

-- get randomly distributed antialiasing rays
-- Parameters : infinite list of random numbers, ray number, stepX, stepY, base direction
-- Return : (list of antialiasing ray directions, tail of the random numbers' list)
randomAL :: [Float] -> Integer -> Vec3Df -> Vec3Df -> Vec3Df -> ([Vec3Df], [Float])
randomAL rlist n stepX stepY dir = let stepx = mul 0.5 stepX
                                       stepy = mul 0.5 stepY
                                 in
                                  if n == 1
                                  then ([dir],rlist)
                                  else let (tuples, rlist2) = listRandomTuples n rlist 
                                       in (map (\t -> dir + mul (fst t * 2 - 1) stepx + mul (snd t * 2 - 1) stepy) tuples, rlist2)

                                     
intToFloat :: Int -> Float
intToFloat i = fromInteger $ toInteger i

-- Get all the pixels
getPixels :: Int -> Int -> [Point]
getPixels h w = [(x,y) | x <- [0..h-1], y<- [0..w-1]]

-- Set the color of pixels, no random edition 
setPixels' :: [Point] -> (Point -> Color) -> Image -> IO ()
setPixels' [] _ _ = return () 
setPixels' (p:t) f im = do
    setPixel p (f p) im
    setPixels' t f im

-- Set the color of pixels
-- Parameters : image points, random numbers list, color function
-- the color function use the list of random numbers, and returns its tail after having used all the algorithm required random values
setPixels :: [Point] -> [Float] -> ([Float] -> Point -> (Color,[Float])) -> Image -> IO ()
setPixels [] _ _ _ = return () 
setPixels (p:t) rlist f im = do
    ;let (c,rlist2) = f rlist p
    setPixel p c im
    setPixels t rlist2 f im

take' :: Int -> Int -> [Float] -> [Float]
take' i1 i2 l = take (i2 - i1) $ drop i1 l

-- h : height, w : width of the image, n: antialiasing parameter (1 5 or 9)
main :: Int -> Int -> Integer -> IO()
main h w n = do
  im <- newImage (h,w)
  gen <- getStdGen
  ;let l = randomRs (0, 1) gen
  setPixels (getPixels h w) l (rayTrace (intToFloat h) (intToFloat w) (Vec3Df 1 0 0) (Vec3Df 0 1 0) n createScene4) im
  savePngFile "result.png" im
  return ()

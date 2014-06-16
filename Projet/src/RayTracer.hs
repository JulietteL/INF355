module RayTracer where

import Graphics.GD
import Vector
import Scene
import Ray
import Scene
import Brdf

createScene :: Scene
createScene = let cam = Camera (Vec3Df 0 0 (-4)) (Vec3Df 0 0 0)
                  objs = [Sphere (Vec3Df 0 0 0) 1 ( Material (Vec3Df 255 0 0)),
                          Plan (Vec3Df 0 0 1) (Vec3Df 0 0 (-1)) (Material (Vec3Df 0 255 0)),
                          Sphere (Vec3Df 0 (-0.5) (-0.5)) 0.7 (Material (Vec3Df 0 0 255))]
                  lights = [Light (Vec3Df 0 4 (-4)) (Vec3Df 1 1 1)]
                  in (cam, objs, lights)
                     
tanX :: Float -> Float -> Float
tanX h w = tan (80 * pi/180) * w/h 
tanY :: Float
tanY = tan (80 * pi/180)

-- rayTrace: height width rightVector upVector antialiasingRays Scene imagePixel -> pixel color
rayTrace :: Float -> Float -> Vec3Df -> Vec3Df -> Integer -> Scene -> Point -> Color
rayTrace h w rv uv n ((Camera o t),objs, lights) (x,y) = let stepX = mul ((tanX w h)/h) rv
                                                             stepY = mul (tanY/w) uv
                                      in let dir = t - o + (mul (intToFloat x - h/2) stepX) + (mul (intToFloat y - w/2) stepY)
                                         in let dirs = antiAliasing n stepX stepY dir
                                            in toColor $ divl (intToFloat $ length dirs) (sum $ [toVec3Df $ brdf objs lights (Ray o (normalize d)) | d <- dirs])


                                          --brdf objs lights (Ray o (normalize dir)) 

-- uniform rays
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

intToFloat :: Int -> Float
intToFloat i = fromInteger $ toInteger i

-- Get all the pixels
getPixels :: Int -> Int -> [Point]
getPixels h w = [(x,y) | x <- [0..h-1], y<- [0..w-1]]

-- Set the color of pixels
setPixels :: [Point] -> (Point -> Color) -> Image -> IO ()
setPixels [] _ _ = return () 
setPixels (p:t) f im = do
    setPixel p (f p) im
    setPixels t f im
                     
-- h : height, w : width of the image, n: antialiasing parameter (1 5 or 9)
main :: Int -> Int -> Integer -> IO()
main h w n = do
  im <- newImage (h,w)
  setPixels (getPixels h w) (rayTrace (intToFloat h) (intToFloat w) (Vec3Df 1 0 0) (Vec3Df 0 1 0) n createScene) im
  savePngFile "result.png" im
  return ()

main' :: Int -> Int -> IO()
main' h w = main h w 1  

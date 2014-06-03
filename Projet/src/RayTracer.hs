module RayTracer where

import Graphics.GD
import Vector
import Scene
import Ray
import Scene

createScene :: Scene
createScene = let cam = Camera (Vec3Df 0 0 (-10)) (Vec3Df 0 0 0)
                  objs = [Sphere (Vec3Df 0 0 0) 1]
                  in (cam, objs)

tanX :: Float -> Float -> Float
tanX h w = tan (80 * pi/180) * w/h 
tanY :: Float
tanY = tan (80 * pi/180)

rayTrace :: Float -> Float -> Vec3Df -> Vec3Df -> Scene -> Point -> Color
rayTrace h w rv uv ((Camera o t),objs) (x,y) = let stepX = mul ((intToFloat x - w/2)/w * (tanX h w)) rv
                                                   stepY = mul ((intToFloat y - h/2)/h * tanY) uv
                                      in let dir = t - o + stepX + stepY
                                         in brdf objs (Ray o (normalize dir)) 

brdf :: [Object] -> Ray -> Color
brdf objs (Ray o d) = rgb 0 0 0 

getPixels :: Int -> Int -> [Point]
getPixels h w = [(x,y) | x <- [0..h-1], y<- [0..w-1]]

setPixels :: [Point] -> (Point -> Color) -> Image -> IO ()
setPixels [] _ _ = return () 
setPixels (p:t) f im = do
  setPixel p (f p) im
  setPixels t f im                     

intToFloat :: Int -> Float
intToFloat i = fromInteger $ toInteger i

-- h : height, w : width of the image
main :: Int -> Int -> IO()
main h w = do
  im <- newImage (h,w)
  setPixels (getPixels w h) (rayTrace (intToFloat h) (intToFloat w) (Vec3Df 1 0 0) (Vec3Df 0 1 0) createScene) im
  savePngFile "result.png" im
  return ()
  

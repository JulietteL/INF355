module RayTracer where

import Graphics.GD
import Vector
import Scene
import SceneExamples
import Ray
import Scene
import Brdf

mat0 :: Vec3Df -> Material
mat0 c = SpecularMaterial c 1 (Vec3Df 0 0 0) 0 1

mat1 :: Vec3Df ->Material
mat1 c = SpecularMaterial c 1 (Vec3Df 255 255 255) 0.3 10

mat2 :: Vec3Df ->Material
mat2 c = SpecularMaterial c 1 (Vec3Df 255 255 255) 0.9 50

tanX :: Float -> Float -> Float
tanX h w = tan (80 * pi/180) * w/h 
tanY :: Float
tanY = tan (80 * pi/180)

rayTrace :: Float -> Float -> Vec3Df -> Vec3Df -> Scene -> Point -> Color
rayTrace h w rv uv ((Camera o t),objs, lights) (x,y) = let stepX = mul ((intToFloat x - h/2)/h * (tanX w h)) rv
                                                           stepY = mul ((intToFloat y - w/2)/w * tanY) uv
                                      in let dir = t - o + stepX + stepY
                                         in brdf objs lights (Ray o (normalize dir)) "phong" 

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

toVec3Df :: Color -> Vec3Df
toVec3Df c  = let (r, g, b, _) = toRGBA c
                  in Vec3Df (fromIntegral r) (fromIntegral g) (fromIntegral b)
                     
-- h : height, w : width of the image
main :: Int -> Int -> IO()
main h w = do
  im <- newImage (h,w)
  setPixels (getPixels h w) (rayTrace (intToFloat h) (intToFloat w) (Vec3Df 1 0 0) (Vec3Df 0 1 0) createScene2) im
  savePngFile "result.png" im
  return ()

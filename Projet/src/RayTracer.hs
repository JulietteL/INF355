module RayTracer where

import Data.Maybe
import Graphics.GD
import Vector
import Scene
import Ray
import Scene

createScene :: Scene
createScene = let cam = Camera (Vec3Df 0 0 (-10)) (Vec3Df 0 0 0)
                  objs = [Sphere (Vec3Df 0 0 0) 1]
                  in (cam, objs)


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

brdf :: Scene -> Ray -> Color
brdf (_, objs) (Ray o d) = sum( fmap (\r -> if isJust r then rgb 255 255 255 else rgb 0 0 0 )[intersect (Ray o d) obj | obj <- objs ])
  
-- Raytracing function
rayTrace ::  Scene -> Point -> Color
rayTrace  (cam, objs) (x,y) = rgb x y 0

-- h : height, w : width of the image
main :: Int -> Int -> IO()
main h w = do
  im <- newImage (h,w)
  setPixels (getPixels w h) (rayTrace createScene ) im
  savePngFile "result.png" im
  

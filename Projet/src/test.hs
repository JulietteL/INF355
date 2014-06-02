module Test where

import Graphics.GD

rayTrace ::  Point -> Color
rayTrace (x,y) = rgb x y 0

getPixels :: Int -> Int -> [Point]
getPixels h w = [(x,y) | x <- [0..h-1], y<- [0..w-1]]

setPixels :: [Point] -> (Point -> Color) -> Image -> IO ()
setPixels [] _ _ = return () 
setPixels (p:t) f im = do
  setPixel p (f p) im
  setPixels t f im
  
main :: Int -> Int -> IO ()
main w h= do
  im <- newImage (h, w)
  setPixels (getPixels w h) rayTrace im
  savePngFile "test.png" im

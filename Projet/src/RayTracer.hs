module RayTracer where

import Graphics.GD
import Vector
import Scene
import Ray

-- Ray
data Ray = Ray Vec3Df Vec3Df
           deriving Show

createScene :: Scene
createScene = let cam = Camera (Vec3Df 0 0 (-10)) (Vec3Df 0 0 0)
                  objs = [Sphere (Vec3Df 0 0 0) 1]
              in (cam, objs)

main :: Int -> Int -> IO()
main h w = do
  return ()
  

module Vector where

import Graphics.GD

type Vec3Di =  (Int, Int, Int)
data Vec3Df = Vec3Df Float Float Float
              deriving Show

instance Num Vec3Df where
  (Vec3Df x1 y1 z1) + (Vec3Df x2 y2 z2) = Vec3Df (x1+ x2) (y1 + y2) (z1 + z2)
  (Vec3Df x1 y1 z1) - (Vec3Df x2 y2 z2) = Vec3Df (x1- x2) (y1 - y2) (z1 - z2)
  (Vec3Df x1 y1 z1) * (Vec3Df x2 y2 z2) = Vec3Df (x1*x2) (y1*y2) (z1 *z2)
  signum (Vec3Df x y z) = Vec3Df (signum x) (signum y) (signum z)
  abs (Vec3Df x y z) = Vec3Df (abs x) (abs y) (abs z)
  fromInteger a = Vec3Df (fromInteger a) (fromInteger a) (fromInteger a)

dot :: Vec3Df -> Vec3Df -> Float
dot (Vec3Df x1 y1 z1) (Vec3Df x2 y2 z2) = x1*x2 + y1*y2 + z1*z2

mul :: Float -> Vec3Df -> Vec3Df
mul a (Vec3Df x y z) = Vec3Df (a*x) (a*y) (a*z)

mur :: Vec3Df -> Float -> Vec3Df
mur v a = mul a v

squaredNorm :: Vec3Df -> Float
squaredNorm v = dot v v

norm :: Vec3Df -> Float
norm = sqrt.squaredNorm

normalize :: Vec3Df -> Vec3Df
normalize v = if (norm v == 0) then v
              else mul (1/(norm v)) v


toVec3Df :: Color -> Vec3Df
toVec3Df c  = let (r, g, b, _) = toRGBA c
                  in Vec3Df (fromIntegral r) (fromIntegral g) (fromIntegral b)

toColor :: Vec3Df -> Color
toColor (Vec3Df r g b) = rgb (round r) (round g) (round b) 

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

instance Eq Vec3Df where
  (Vec3Df x1 y1 z1) == (Vec3Df x2 y2 z2) = x1 == x2 && y1 == y2 && z1 == z2

instance Ord Vec3Df where
  a <= b = squaredNorm a <= squaredNorm b

dot :: Vec3Df -> Vec3Df -> Float
dot (Vec3Df x1 y1 z1) (Vec3Df x2 y2 z2) = x1*x2 + y1*y2 + z1*z2

cross :: Vec3Df -> Vec3Df -> Vec3Df
cross (Vec3Df x1 y1 z1) (Vec3Df x2 y2 z2) = (Vec3Df (y1*z2-z1*y2) (z1*x2-x1*z2) (x1*y2-y1*x2))

mul :: Float -> Vec3Df -> Vec3Df
mul a (Vec3Df x y z) = Vec3Df (a*x) (a*y) (a*z)

mur :: Vec3Df -> Float -> Vec3Df
mur v a = mul a v

divl :: Float -> Vec3Df -> Vec3Df
divl a v = mul (1/a) v

divr :: Vec3Df -> Float -> Vec3Df
divr v a = mur v (1/a)

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
toColor (Vec3Df r g b) = rgb (round $ min 255 r) (round $ min 255 g) (round $ min 255 b)

toCartesian :: Vec3Df -> Vec3Df
toCartesian (Vec3Df r phi theta) = Vec3Df (r*sin(phi)*cos(theta)) (r*sin(phi)*sin(theta)) (r*cos(phi))

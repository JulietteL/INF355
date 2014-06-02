module Ray where

import Vector
import Scene

-- Ray
data Ray = Ray Vec3Df Vec3Df
           deriving Show

intersect :: Ray -> Sphere -> Maybe (Vec3Df, Vec3Df)
intersect (Ray o d) (Sphere ce r) = let
  a = squaredNorm d
  b = dot d (o-ce)
  c = squaredNorm (o-ce) - (r**2)
  delta = b**2 - a*c
                                   in if (delta <  0) then Nothing else let
    t1 = (-b + sqrt delta)/a
    t2 = (-b - sqrt delta)/a
    in if (t1 < t2 && t1 > 0) then Just (o + (mul t1 d), o) else if (t2 < t1 && t2 > 0) then Just(o + (mul t2 d), o) else Nothing


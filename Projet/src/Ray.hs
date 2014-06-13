module Ray where

import Vector
import Scene

-- Ray
data Ray = Ray Vec3Df Vec3Df
           deriving Show

intersect :: Ray -> Object -> Maybe (Vec3Df, Vec3Df)
intersect (Ray o d) (Sphere ce r _) = let
  a = squaredNorm d
  b = dot d (o-ce)
  c = squaredNorm (o-ce) - (r**2)
  delta = b**2 - a*c
                                   in if (delta <  0) then Nothing else let
    t1 = (-b + sqrt delta)/a
    t2 = (-b - sqrt delta)/a
    in if (t1 <= t2 && t1 > 0) then
         let p1 = o + (mul t1 d)
         in Just (p1, p1 - ce)
       else if (t2 < t1 && t2 > 0) then
              let p2 = o + (mul t2 d)
              in Just(p2, normalize $ p2 - ce)
            else Nothing
intersect (Ray o d) (Plan p n _) =
  if (dot d n == 0) then Nothing
  else let t = (1/(dot d n)) * (dot (p-o) n)
       in if (t < 0) then Nothing
          else Just(o + mul t d, normalize n )


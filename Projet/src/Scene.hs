module Scene where

import  Vector

-- Camera position target
data Camera = Camera Vec3Df Vec3Df

--data Object = Sphere | Plan
-- Sphere center radius
data Sphere = Sphere Vec3Df Float
-- Plan point normal
data Plan = Plan Vec3Df Vec3Df


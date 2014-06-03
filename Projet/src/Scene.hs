module Scene where

import  Vector

-- Camera position target
data Camera = Camera Vec3Df Vec3Df

data Object = Sphere Vec3Df Float | Plan Vec3Df Vec3Df

type Scene = (Camera, [Object])

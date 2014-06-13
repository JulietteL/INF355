module Scene where

import  Vector

-- Camera position target
data Camera = Camera Vec3Df Vec3Df

--Objects types to be rendered :
-- Sphere : center, radius
-- Plan : point, normal
data Object = Sphere Vec3Df Float Material | Plan Vec3Df Vec3Df Material

getMaterial :: Object -> Material
getMaterial (Sphere _ _ mat) = mat
getMaterial (Plan _ _ mat) = mat

-- Lumière : position, couleur
data Light = Light Vec3Df Vec3Df

type Scene = (Camera, [Object], [Light])

-- Material for object
-- Material : diffuse color
data Material = Material Vec3Df

getDiffuseColor :: Material -> Vec3Df
getDiffuseColor (Material c) = c

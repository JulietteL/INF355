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
data Light = Light Vec3Df Vec3Df | ExtendedLight Vec3Df Vec3Df Float

getPointsOnLight :: Light -> Int -> [Light]
getPointsOnLight l 1 = [l]
getPointsOnLight (Light p c) _ = [Light p c]
getPointsOnLight l n = let (ExtendedLight p c r) = l
                       in concat [[Light p c] , getPointsOnLight l $ n-1]

randomPointOnLight :: Light -> Vec3Df
randomPointOnLight (ExtendedLight p _ r) = let theta  = 3.14
                                               phi = 1.50
                                               in Vec3Df theta phi r
randomPointOnLight (Light p c) = p

type Scene = (Camera, [Object], [Light])

-- Material for object
-- Material : diffuse color
-- Material : diffuse color, diffuse coeff, specular color, specular coeff, shininess
data Material = DiffuseMaterial Vec3Df | SpecularMaterial Vec3Df Float Vec3Df Float Float

getDiffuseColor :: Material -> Vec3Df
getDiffuseColor (DiffuseMaterial d) = d
getDiffuseColor (SpecularMaterial d _ _ _ _) = d

getDiffuseCoeff :: Material -> Float
getDiffuseCoeff (SpecularMaterial _ dc _ _ _) = dc
getDiffuseCoeff _ = error "no diffuse coefficient"
getSpecularColor :: Material -> Vec3Df
getSpecularColor (SpecularMaterial _ _ s _ _) = s
getSpecularColor _ = error "no specular color"

getSpecularCoeff :: Material -> Float
getSpecularCoeff (SpecularMaterial _ _ _ sc _) = sc
getSpecularCoeff _ = error "no specular coefficient"

getShininess :: Material -> Float
getShininess (SpecularMaterial _ _ _ _ sh) = sh
getShininess _ = error "no shininess coefficient"

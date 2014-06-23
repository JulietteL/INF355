
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

getPointsOnLight :: Light -> Int -> [Float] -> ([Light], [Float])
getPointsOnLight l 1 list = ([l], list)
getPointsOnLight (Light p c) _ list = ([Light p c], list)
getPointsOnLight (ExtendedLight p c r) n list =
  let (newPos, newList) = randomPointOnLight (ExtendedLight p c r) list
      (lights, newList') = getPointsOnLight (ExtendedLight p c r) (n-1) newList
  in (((Light newPos c) : lights), newList') 

randomPointOnLight :: Light -> [Float] -> (Vec3Df, [Float])
randomPointOnLight (ExtendedLight p _ r) list = let ([r1, r2], newList) = splitAt 2 list
                                               in (p + (toCartesian $ Vec3Df r (r1*2*pi) (r2*pi)), newList)
randomPointOnLight (Light p c) list = (p, list)

type Scene = (Camera, [Object], [Light])

-- Material for object
-- Material : diffuse color
-- Material : diffuse color, diffuse coeff, specular color, specular coeff, shininess
-- Material : reflexion coeff, diffuse color, diffuse coeff, specular color, specular coeff, shininess
data Material = DiffuseMaterial Vec3Df | SpecularMaterial Vec3Df Float Vec3Df Float Float | ReflexiveMaterial Float Vec3Df Float Vec3Df Float Float

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

getReflexiveCoeff :: Material -> Float
getReflexiveCoeff (ReflexiveMaterial r _ _ _ _ _) = r
getReflexiveCoeff _ = 0

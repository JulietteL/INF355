
module Scene where

import  Vector

-- Camera position target
data Camera = Camera Vec3Df Vec3Df

-- Objects types to be rendered :
-- Sphere : center, radius
-- Plan : point, normal
data Object = Sphere Vec3Df Float Material | Plan Vec3Df Vec3Df Material

getMaterial :: Object -> Material
getMaterial (Sphere _ _ mat) = mat
getMaterial (Plan _ _ mat) = mat

-- Light : position, color
data Light = Light Vec3Df Vec3Df | ExtendedLight Vec3Df Vec3Df Float

-- Utility function
-- n: number of tuples
-- l: infinite randomList
-- returns list of tuples the rest of the infinite randomList
listRandomTuples :: Integer -> [Float] -> ([(Float,Float)],[Float])
listRandomTuples n l = let sp = splitAt (2 * fromInteger n) l
                           sp1 = splitAt (fromInteger n) (fst sp)
                       in (zip (fst sp1) (snd sp1), snd sp)


-- Get random points on a spherical extended light
-- Parameters : light, number of points, random numbers list
-- Return : (set of points described as punctual lights, tail of the random numbers' list)
getPointsOnLight :: Light -> Int -> [Float] -> ([Light], [Float])
getPointsOnLight (Light p c) _ list = ([Light p c], list)
getPointsOnLight (ExtendedLight p c r) n list = let (tuples,list') = listRandomTuples (fromIntegral n) list
                                                in (map (\x -> (Light (p + toCartesian (Vec3Df r (fst x * 2*pi) (snd x * pi))) c)) tuples, list')

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

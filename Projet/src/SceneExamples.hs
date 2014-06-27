module SceneExamples where

import Scene
import Vector

-- Materials examples

-- Material of type specular with no specularity
mat0 :: Vec3Df -> Material
mat0 c = SpecularMaterial c 1 (Vec3Df 0 0 0) 0 1

mat1 :: Vec3Df ->Material
mat1 c = SpecularMaterial c 1 (Vec3Df 255 255 255) 0.3 10

mat2 :: Vec3Df -> Material
mat2 c = SpecularMaterial c 1 (Vec3Df 255 255 255) 0.9 50

mat3 :: Float -> Vec3Df -> Material
mat3 f c = ReflexiveMaterial f c 1 (Vec3Df 255 255 255) 0.9 5

-- Scenes
createScene :: Scene
createScene = let cam = Camera (Vec3Df 0 0 (-4)) (Vec3Df 0 0 0)
                  objs = [Sphere (Vec3Df 0 0 0) 1 (mat1 $ Vec3Df 255 0 0),
                          Plan (Vec3Df 0 0 1) (Vec3Df 0 0 (-1)) (mat1 $ Vec3Df 0 255 0),
                          Sphere (Vec3Df 0 (-0.5) (-0.5)) 0.7 (mat1 $ Vec3Df 0 0 255)]
                  lights = [Light (Vec3Df 0 4 (-4)) (Vec3Df 1 1 1)]
                  in (cam, objs, lights)


createScene2 :: Scene
createScene2 = let cam = Camera (Vec3Df 0 0 (-10)) (Vec3Df 0 0 0)
                   objs = [
                     Sphere (Vec3Df 0 0 0) 1 (mat1 $ Vec3Df 255 0 0),
                     Sphere (Vec3Df 2 2 5) 3 (mat2 $ Vec3Df 30 189 64),
                     Sphere (Vec3Df (-2) (-2) 3) 1.5 (mat2 $ Vec3Df 40 40 220)
                          ]
                   lights = [
                     ExtendedLight (Vec3Df (-50) 0 (-30)) (Vec3Df 1 1 1) 3,
                     ExtendedLight (Vec3Df 50 0 (-30)) (Vec3Df 0.4 0 0.6) 2
                     ]
                  in (cam, objs, lights)

createScene3 :: Scene
createScene3 = let cam = Camera (Vec3Df 3 0 (-10)) (Vec3Df 0 0 0)
                   objs = [
                     Sphere (Vec3Df 0 0 0) 1 (mat1 $ Vec3Df 255 0 0),
                     --Plan (Vec3Df 4 0 0) (Vec3Df (-1) 0 0) (mat3 1 $ Vec3Df 0 255 0)
                     Plan (Vec3Df (-1) 0 0) (Vec3Df 1 0 0) (mat3 0.8 $ Vec3Df 0 255 0)
                          ]
                   lights = [Light (Vec3Df 0 5 (-4)) (Vec3Df 1 1 1)]
                  in (cam, objs, lights)

createScene4 :: Scene
createScene4 = let cam = Camera (Vec3Df 0 0 (-10)) (Vec3Df 0 0 0)
                   objs = [
                     Sphere (Vec3Df 0 0 0) 1 (mat1 $ Vec3Df 255 0 0),
                     Sphere (Vec3Df 2 2 5) 3 (mat3 0.8 $ Vec3Df 15 15 115),
                     Sphere (Vec3Df (-2) (-2) 3) 1.5 (mat2 $ Vec3Df 40 40 220),
                     Sphere (Vec3Df (-1) 1 2) 1.2 (mat2 $ Vec3Df 255 230 255),
                     Plan (Vec3Df 0 0 10) (Vec3Df 0 0.5 (-1)) (mat3 0.95 $ Vec3Df 255 255 255)
                     ]
                   lights = [
                     ExtendedLight (Vec3Df (-50) 0 (-30)) (Vec3Df 0.8 0.2 0) 3,
                     ExtendedLight (Vec3Df 50 0 (-30)) (Vec3Df 0.5 0.8 1) 2
                     ]
                  in (cam, objs, lights)
                  
createScene5 :: Scene
createScene5 = let cam = Camera (Vec3Df 0 0 (-10)) (Vec3Df 0 0 0)
                   r = 1.0
                   objs = [
                     Sphere (Vec3Df (-r) 0 0) r (mat3 0.6 $ Vec3Df 64 224 208),
                     Sphere (Vec3Df r 0 0) r (mat3 0.6 $ Vec3Df 50 205 50 ),
                     Sphere (Vec3Df 0 (-r*(sqrt 3)) 0) r (mat3 0.6 $ Vec3Df 154 130 205) ,
                     Plan (Vec3Df 0 r 0) (Vec3Df 0 (-1) 0 ) (mat3 0.85 $ Vec3Df 255 240 240),
                     Plan (Vec3Df (-2*r) 0 0) (Vec3Df 1 0 0 ) (mat3 0.85 $ Vec3Df 255 240 240),
                     Plan (Vec3Df (2*r) 0 0) (Vec3Df (-1) 0 0 ) (mat3 0.85 $ Vec3Df 255 240 240),
                     Plan (Vec3Df 0 0 r) (Vec3Df 0 0 (-1)) (mat3 0.85 $ Vec3Df 255 240 240),
                     Plan (Vec3Df 0 (-r*(1 + sqrt 3)) 0) (Vec3Df 0 1 0 ) (mat3 0.85 $ Vec3Df 255 240 240)
                     ]
                   lights = [Light (Vec3Df 0 0 (-30)) (Vec3Df 1 1 1) ] --ExtendedLight (Vec3Df 0 0 (-30)) (Vec3Df 1 1 1) 5 ]
                  in (cam, objs, lights)

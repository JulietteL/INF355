module SceneExamples where

import Scene
import Vector

-- Materials examples

-- Material of type specular with no specularity
mat0 :: Vec3Df -> Material
mat0 c = SpecularMaterial c 1 (Vec3Df 0 0 0) 0 1

mat1 :: Vec3Df ->Material
mat1 c = SpecularMaterial c 1 (Vec3Df 255 255 255) 0.3 10

mat2 :: Vec3Df ->Material
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
                     Sphere (Vec3Df 1 3 0) 2 (mat2 $ Vec3Df 30 189 64)
                          ]
                   lights = [Light (Vec3Df 0 (-0.5) (-4)) (Vec3Df 1 1 1)]
                  in (cam, objs, lights)

createScene3 :: Scene
createScene3 = let cam = Camera (Vec3Df 3 0 (-10)) (Vec3Df 0 0 0)
                   objs = [
                     Sphere (Vec3Df 0 0 0) 1 (mat1 $ Vec3Df 255 0 0),
                     Plan (Vec3Df 4 0 0) (Vec3Df (-1) 0 0) (mat3 1 $ Vec3Df 0 255 0),
                     Plan (Vec3Df (-1) 0 0) (Vec3Df 1 0 0) (mat3 0.8 $ Vec3Df 0 255 0)
                          ]
                   lights = [Light (Vec3Df 0 5 (-4)) (Vec3Df 1 1 1)]
                  in (cam, objs, lights)

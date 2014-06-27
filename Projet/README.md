#Raytracer in Haskell#

##Description##

This is a basic Raytracer written in Haskell language.

##Features##

Basic primitives : spheres and plans
Diffuse, specular and reflexive materials
Hard and soft shadows 
Antialiasing

##Usage##

Compile "RayTracer.hs" and use the main function
main imageWidth imageHeight antialiasingRays

Some parameters can be modified in the file "Brdf.hs"
- Activate shadows or not
- Number of rays casted to extended lights
- Maximal number of bounces for reflexive materials

Scene samples can be found and created in "SceneExamples.hs"

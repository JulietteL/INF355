module Brdf where

import Data.Maybe
import Graphics.GD
import Scene
import Ray
import Vector

-- Old version
-- brdf :: [Object] -> Ray -> Color
-- brdf objs (Ray o d) = sum( fmap (\r -> if isJust r then rgb 255 255 255 else rgb 0 0 0 )[intersect (Ray o d) obj | obj <- objs ])
  

brdf :: [Object] -> Ray -> Color
brdf objs (Ray o d) = sum( fmap (\(r, mat) -> if isJust r then toColor(getDiffuseColor(mat)) else rgb 0 0 0 )[(intersect (Ray o d) obj, getMaterial(obj)) | obj <- objs ])
  

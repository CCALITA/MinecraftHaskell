module Engine.Specular
  ( specularIntensity
  ) where

import Linear (V3, normalize, dot, (^+^))

-- | Blinn-Phong specular intensity.
--
-- @specularIntensity lightDir viewDir normal shininess@ computes the specular
-- highlight term: halfVec = normalize(lightDir + viewDir),
-- spec = max(0, dot(normal, halfVec)) ^ shininess.
specularIntensity :: V3 Float -> V3 Float -> V3 Float -> Float -> Float
specularIntensity lightDir viewDir normal shininess =
  let halfVec = normalize (lightDir ^+^ viewDir)
      cosAngle = max 0 (dot normal halfVec)
  in cosAngle ** shininess

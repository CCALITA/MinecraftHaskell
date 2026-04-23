module Engine.EntityRender
  ( entityColor
  , buildEntityQuad
  ) where

import Linear (V3(..), (^*))

-- | Get RGBA color for an entity based on its tag
entityColor :: String -> (Float, Float, Float, Float)
entityColor "Zombie"           = (0.2,  0.6,  0.2,  1.0)  -- green
entityColor "Skeleton"         = (0.85, 0.85, 0.85, 1.0)  -- bone white
entityColor "Creeper"          = (0.0,  0.8,  0.0,  1.0)  -- bright green
entityColor "Spider"           = (0.3,  0.2,  0.2,  1.0)  -- dark brown
entityColor "Pig"              = (0.95, 0.7,  0.7,  1.0)  -- pink
entityColor "Cow"              = (0.55, 0.35, 0.2,  1.0)  -- brown
entityColor "Sheep"            = (0.9,  0.9,  0.9,  1.0)  -- white wool
entityColor "Chicken"          = (1.0,  1.0,  1.0,  1.0)  -- white
entityColor "Wolf"             = (0.7,  0.7,  0.7,  1.0)  -- gray
entityColor "TamedWolf"        = (0.4,  0.6,  0.9,  1.0)  -- blue tint
entityColor "TamedWolfSitting" = (0.4,  0.6,  0.9,  1.0)
entityColor "Painting"         = (0.8,  0.65, 0.3,  1.0)  -- gold frame
entityColor "Minecart"         = (0.5,  0.5,  0.5,  1.0)  -- iron gray
entityColor "Boat"             = (0.6,  0.45, 0.25, 1.0)  -- wood brown
entityColor _                  = (1.0,  0.0,  1.0,  1.0)  -- magenta (unknown)

-- | Build 6 vertices (two triangles) for an entity billboard quad.
--   Takes camera right/up vectors, entity position, and entity tag.
--   Returns 42 floats: 6 vertices * 7 components (x,y,z, r,g,b,a).
buildEntityQuad
  :: V3 Float    -- ^ Camera right vector (normalized)
  -> V3 Float    -- ^ Camera up vector (normalized)
  -> V3 Float    -- ^ Entity position
  -> String      -- ^ Entity tag
  -> [Float]
buildEntityQuad camRight camUp' (V3 px' py' pz') tag =
  let -- Billboard half extents
      hw = 0.4 :: Float  -- half width
      hh = 0.9 :: Float  -- half height
      V3 rx ry rz = camRight ^* hw
      V3 ux uy uz = camUp' ^* hh
      -- Center offset: entity pos + half height so feet are at ground level
      cx' = px'; cy' = py' + hh; cz' = pz'
      -- Four corners: bottom-left, bottom-right, top-left, top-right
      blx = cx' - rx - ux; bly = cy' - ry - uy; blz = cz' - rz - uz
      brx = cx' + rx - ux; bry = cy' + ry - uy; brz = cz' + rz - uz
      tlx = cx' - rx + ux; tly = cy' - ry + uy; tlz = cz' - rz + uz
      trx = cx' + rx + ux; try' = cy' + ry + uy; trz = cz' + rz + uz
      -- Color based on mob type
      (cr, cg, cb, ca) = entityColor tag
  in -- Triangle 1: BL, BR, TL
     [ blx, bly, blz, cr, cg, cb, ca
     , brx, bry, brz, cr, cg, cb, ca
     , tlx, tly, tlz, cr, cg, cb, ca
     -- Triangle 2: BR, TR, TL
     , brx, bry, brz, cr, cg, cb, ca
     , trx, try', trz, cr, cg, cb, ca
     , tlx, tly, tlz, cr, cg, cb, ca
     ]

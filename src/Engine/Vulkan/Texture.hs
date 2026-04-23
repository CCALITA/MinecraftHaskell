module Engine.Vulkan.Texture
  ( TextureImage(..)
  , createTextureAtlas
  , createPlaceholderAtlas
  , destroyTextureImage
  ) where

import qualified Vulkan as Vk
import qualified Vulkan.Zero as Vk
import qualified Vulkan.CStruct.Extends as Vk

import Engine.Vulkan.Memory (createBuffer, destroyBuffer, BufferAllocation(..), copyBuffer)

import Data.Bits ((.|.), (.&.), shiftL)
import qualified Data.Bits
import Data.Word (Word8, Word32)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Foreign.Ptr (castPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Storable (sizeOf)

-- | Texture image with sampler for use in descriptor sets
data TextureImage = TextureImage
  { tiImage     :: !Vk.Image
  , tiMemory    :: !Vk.DeviceMemory
  , tiImageView :: !Vk.ImageView
  , tiSampler   :: !Vk.Sampler
  } deriving stock (Show)

-- | Create a placeholder 256x256 texture atlas with patterned tiles for each block type
createPlaceholderAtlas
  :: Vk.PhysicalDevice -> Vk.Device -> Vk.CommandPool -> Vk.Queue
  -> IO TextureImage
createPlaceholderAtlas physDevice device cmdPool queue = do
  let width  = 256 :: Word32
      height = 256 :: Word32
      tileSize = 16 :: Int
      pixels = VS.generate (fromIntegral width * fromIntegral height * 4) $ \i ->
        let pixelIdx = i `div` 4
            channel  = i `mod` 4
            px = pixelIdx `mod` fromIntegral width
            py = pixelIdx `div` fromIntegral width
            tileX = px `div` tileSize
            tileY = py `div` tileSize
            tileIdx = tileX + tileY * (fromIntegral width `div` tileSize)
            localX = px `mod` tileSize
            localY = py `mod` tileSize
        in tilePixel tileIdx localX localY channel :: Word8

  createTextureFromPixels physDevice device cmdPool queue width height pixels

-- | Simple hash for pixel-level noise (deterministic)
pixHash :: Int -> Int -> Int -> Int
pixHash x y s =
  let h0 = (x * 374761393 + y * 668265263 + s * 1274126177) `mod` 0x7FFFFFFF
      h1 = ((h0 `xor` (h0 `shiftR` 13)) * 1103515245 + 12345) `mod` 0x7FFFFFFF
  in abs h1
  where
    xor = Data.Bits.xor
    shiftR = Data.Bits.shiftR

-- | Clamp an Int to valid Word8 range and convert
clampByte :: Int -> Word8
clampByte n = fromIntegral (max 0 (min 255 n))

-- | Generate a pixel for a tile with patterns
tilePixel :: Int -> Int -> Int -> Int -> Word8
tilePixel tileIdx lx ly channel =
  let (r, g, b, a) = tileFull tileIdx lx ly
  in case channel of { 0 -> r; 1 -> g; 2 -> b; 3 -> a; _ -> 0 }

-- | Full RGBA for a pixel within a tile
tileFull :: Int -> Int -> Int -> (Word8, Word8, Word8, Word8)
tileFull tileIdx lx ly = case tileIdx of
  -- Row 0: tileX 0-15
  0  -> grassTop lx ly         -- grass top (V2 0 0)
  1  -> stonePattern lx ly     -- stone (V2 1 0)
  2  -> dirtPattern lx ly      -- dirt (V2 2 0)
  3  -> grassSide lx ly        -- grass side (V2 3 0)
  4  -> planksPattern lx ly    -- oak planks / sand (V2 4 0)
  5  -> gravelPattern lx ly    -- gravel (V2 5 0)
  7  -> brickPattern lx ly     -- brick (V2 7 0)
  8  -> tntSide lx ly          -- TNT side (V2 8 0)
  9  -> tntTop lx ly           -- TNT top (V2 9 0)
  10 -> tntBottom lx ly        -- TNT bottom (V2 10 0)
  13 -> waterPattern lx ly     -- water (V2 13 0)
  14 -> lavaPattern lx ly      -- lava (V2 14 0)
  15 -> glassPattern lx ly     -- glass (V2 15 0)
  -- Row 1: tileX 0-15, tileY 1 → tileIdx 16-31
  16 -> cobblePattern lx ly    -- cobblestone (V2 0 1)
  17 -> bedrockPattern lx ly   -- bedrock (V2 1 1)
  20 -> logBark lx ly          -- oak log bark (V2 4 1)
  21 -> logTop lx ly           -- oak log cross-section (V2 5 1)
  22 -> leavesPattern lx ly    -- oak leaves (V2 6 1)
  25 -> chestPattern lx ly     -- chest (V2 9 1)
  30 -> ladderPattern lx ly    -- ladder (V2 14 1)
  -- Row 2: tileIdx 32-47
  32 -> orePattern lx ly (255, 215, 0)    -- gold ore (V2 0 2)
  33 -> orePattern lx ly (200, 170, 130)  -- iron ore (V2 1 2)
  34 -> orePattern lx ly (40, 40, 40)     -- coal ore (V2 2 2)
  44 -> furnaceSide lx ly      -- furnace side (V2 12 2)
  45 -> furnaceSide lx ly      -- furnace side alt (V2 13 2)
  -- Row 3: tileIdx 48-63
  48 -> woolPattern lx ly         -- wool (V2 0 3)
  49 -> glassPattern lx ly     -- glass alt (V2 1 3)
  50 -> orePattern lx ly (100, 220, 255)  -- diamond ore (V2 2 3)
  54 -> stoneBrickPattern lx ly -- stone brick (V2 6 3)
  59 -> craftingTop lx ly      -- crafting table top (V2 11 3)
  62 -> furnaceTop lx ly       -- furnace top (V2 14 3)
  -- Row 4: tileIdx 64-79
  66 -> snowPattern lx ly      -- snow (V2 2 4)
  72 -> clayPattern lx ly      -- clay (V2 8 4)
  -- Row 5: tileIdx 80+
  80 -> torchPattern lx ly     -- torch (V2 0 5)
  81 -> ironDoorClosedPattern lx ly -- iron door closed (V2 1 5)
  82 -> ironDoorOpenPattern lx ly   -- iron door open (V2 2 5)
  83 -> cactusPattern lx ly    -- cactus (V2 3 5)
  -- New block textures (tiles 27-38)
  27 -> obsidianPattern lx ly      -- obsidian (V2 11 1)
  28 -> oakDoorClosedPattern lx ly -- oak door closed (V2 12 1)
  29 -> oakDoorOpenPattern lx ly   -- oak door open (V2 13 1)
  30 -> ladderPattern lx ly        -- ladder (V2 14 1)
  31 -> bedPattern lx ly           -- bed (V2 15 1)
  35 -> oakFencePattern lx ly      -- oak fence (V2 3 2)
  36 -> farmlandPattern lx ly      -- farmland (V2 4 2)
  37 -> wheatCropPattern lx ly     -- wheat crop (V2 5 2)
  38 -> oakSaplingPattern lx ly    -- oak sapling (V2 6 2)
  39 -> fenceGateClosedPattern lx ly -- fence gate closed (V2 7 2)
  40 -> fenceGateOpenPattern lx ly   -- fence gate open (V2 8 2)
  41 -> leverPattern lx ly          -- lever (V2 9 2)
  42 -> redstoneDustPattern lx ly   -- redstone dust (V2 10 2)
  46 -> trapdoorClosedPattern lx ly  -- trapdoor closed (V2 14 2)
  47 -> trapdoorOpenPattern lx ly    -- trapdoor open (V2 15 2)
  43 -> firePattern lx ly              -- fire (V2 11 2)
  84 -> sugarCanePattern lx ly       -- sugar cane (V2 4 5)
  85 -> pistonTopPattern lx ly       -- piston top/face (V2 5 5)
  86 -> pistonSidePattern lx ly      -- piston side (V2 6 5)
  87 -> pistonHeadPattern lx ly      -- piston head (V2 7 5)
  88 -> railPattern lx ly              -- rail (V2 8 5)
  89 -> dispenserFrontPattern lx ly  -- dispenser front (V2 9 5)
  90 -> enchantingTableTop lx ly     -- enchanting table top
  91 -> enchantingTableSide lx ly    -- enchanting table side
  -- Nether and ore block textures
  92 -> netherrackPattern lx ly      -- netherrack (V2 10 5)
  93 -> soulSandPattern lx ly        -- soul sand (V2 11 5)
  94 -> glowstonePattern lx ly       -- glowstone (V2 12 5)
  95 -> netherBrickPattern lx ly     -- nether brick (V2 13 5)
  96 -> netherPortalPattern lx ly    -- nether portal (V2 14 5)
  51 -> orePattern lx ly (180, 0, 0)     -- redstone ore (V2 3 3)
  52 -> orePattern lx ly (30, 70, 180)   -- lapis ore (V2 4 3)
  53 -> orePattern lx ly (60, 200, 80)   -- emerald ore (V2 5 3)
  67 -> mossyCobblePattern lx ly     -- mossy cobblestone (V2 3 4)
  68 -> mossyStoneBrickPattern lx ly -- mossy stone brick (V2 4 4)
  -- Row 6: wood variants and flora (tileIdx 96-111)
  96 -> birchBark lx ly              -- birch log bark (V2 0 6)
  97 -> birchLogTop lx ly            -- birch log cross-section (V2 1 6)
  98 -> birchLeavesPattern lx ly     -- birch leaves (V2 2 6)
  99 -> birchPlanksPattern lx ly     -- birch planks (V2 3 6)
  100 -> spruceBark lx ly            -- spruce log bark (V2 4 6)
  101 -> spruceLogTop lx ly          -- spruce log cross-section (V2 5 6)
  102 -> spruceLeavesPattern lx ly   -- spruce leaves (V2 6 6)
  103 -> sprucePlanksPattern lx ly   -- spruce planks (V2 7 6)
  104 -> jungleBark lx ly            -- jungle log bark (V2 8 6)
  105 -> jungleLogTop lx ly          -- jungle log cross-section (V2 9 6)
  106 -> jungleLeavesPattern lx ly   -- jungle leaves (V2 10 6)
  107 -> junglePlanksPattern lx ly   -- jungle planks (V2 11 6)
  108 -> tallGrassPattern lx ly      -- tall grass (V2 12 6)
  109 -> dandelionPattern lx ly      -- dandelion (V2 13 6)
  110 -> rosePattern lx ly           -- rose (V2 14 6)
  111 -> brownMushroomPattern lx ly  -- brown mushroom (V2 15 6)
  -- Row 7: continued flora
  112 -> redMushroomPattern lx ly    -- red mushroom (V2 0 7)
  113 -> bookshelfSide lx ly         -- bookshelf side (V2 1 7)
  114 -> anvilPattern lx ly          -- anvil (V2 2 7)
  115 -> brewingStandPattern lx ly   -- brewing stand (V2 3 7)
  116 -> icePattern lx ly            -- ice (V2 4 7)
  117 -> packedIcePattern lx ly      -- packed ice (V2 5 7)
  118 -> redstoneLampPattern lx ly   -- redstone lamp (V2 6 7)
  119 -> hopperPattern lx ly         -- hopper side (V2 7 7)
  120 -> hopperTopPattern lx ly      -- hopper top opening (V2 8 7)
  -- Fallback: checkerboard pattern so missing tiles are visible
  _  -> let checker = (lx + ly) `mod` 2 == 0
        in if checker then (200, 0, 200, 255) else (100, 0, 100, 255)
  where

    -- Grass top: blade-like patterns with directional vertical grain
    grassTop x y =
      let -- Vertical blade shapes: hash based on x to create columns of blades
          bladeHash = pixHash x 0 42 `mod` 16
          -- Each blade column has a slightly different phase
          bladePhase = pixHash x 0 43 `mod` 4
          -- Vertical grain: alternating light/dark based on blade column
          isLightBlade = (x + bladePhase) `mod` 3 /= 0
          -- Per-pixel noise for subtle variation
          n = pixHash x y 44 `mod` 20
          -- Blade tip darkness (top rows slightly different)
          tipDarken = if y <= 1 && bladeHash < 6 then 10 else 0
          (br, bg, bb) = if isLightBlade
                         then (50 + fromIntegral n `div` 2 - tipDarken, 180 - fromIntegral n - tipDarken, 30 + fromIntegral (n `div` 3))
                         else (30 + fromIntegral n `div` 3 - tipDarken, 140 - fromIntegral (n `div` 2) - tipDarken, 20 + fromIntegral (n `div` 4))
      in (br, bg, bb, 255)

    -- Stone: multi-tone gray layers with subtle cracks
    stonePattern x y =
      let -- Large-scale tone variation (÷4) for layered geology
          largeTone = pixHash (x `div` 4) (y `div` 4) 100 `mod` 40
          -- Medium-scale variation for patches
          medTone   = pixHash (x `div` 2) (y `div` 2) 102 `mod` 20
          -- Small-scale per-pixel grain
          grain     = pixHash x y 103 `mod` 12
          -- Thin dark crack lines (÷3 hash < threshold)
          crackH    = pixHash (x `div` 3) (y `div` 3) 101 `mod` 20
          isCrack   = crackH < 3
          -- Horizontal sediment lines every ~5 pixels
          sediment  = pixHash x (y `div` 5) 104 `mod` 15
          isSediment = sediment < 2
          -- Compose base value: 110-150 range with layered noise
          baseVal   = 110 + largeTone - medTone `div` 2 + grain
          v         = if isCrack then fromIntegral (max 80 (baseVal - 35))
                      else if isSediment then fromIntegral (max 90 (baseVal - 15))
                      else fromIntegral (min 155 baseVal)
      in (v, v, v, 255)

    -- Dirt: brown base with pebble spots and root-like veins
    dirtPattern x y =
      let n = pixHash x y 200 `mod` 100
          -- Base brown color in 100-150 range
          baseVal = 100 + (n * 50) `div` 100
          -- Pebble spots: lighter dots at ~5% of pixels
          isPebble = pixHash x y 201 `mod` 100 < 5
          -- Root-like darker veins: thin horizontal/diagonal lines via hash
          veinHash = pixHash (x `div` 2) (y + x `div` 4) 202 `mod` 20
          isVein = veinHash < 2
          rv | isPebble  = fromIntegral (baseVal + 20)  -- lighter pebble
             | isVein    = fromIntegral (80 + pixHash x y 203 `mod` 11)  -- dark vein 80-90
             | otherwise = fromIntegral baseVal
          gv = rv * 60 `div` 140
          bv = rv * 35 `div` 140
      in (rv, gv, bv, 255)

    -- Grass side: smoother gradient from green top to dirt bottom with irregular transition
    grassSide x y
      | y <= 2    = grassTop x y       -- green top rows
      | y >= 5    = dirtPattern x y    -- dirt bottom rows
      | otherwise =
          -- Transition zone (y=3-4): irregular edge using noise
          let edgeNoise = pixHash x y 300 `mod` 10
              -- At y=3, mostly green; at y=4, mostly dirt
              greenThreshold = if y == 3 then 7 else 3
          in if edgeNoise < greenThreshold then grassTop x y else dirtPattern x y

    -- Oak planks: warm tan, 4 horizontal plank divisions with wood grain
    planksPattern x y =
      let plankIdx = y `div` 4
          plankVar = pixHash 0 plankIdx 400 `mod` 20
          grainLine = y `mod` 4 == 0
          grain = pixHash x y 401 `mod` 100
          noise = pixHash x y 402 `mod` 10
          baseVal = if grainLine then 155 + plankVar
                    else 185 + plankVar + (grain `div` 20)
          r = fromIntegral (min 255 (baseVal + noise)) :: Word8
          g = fromIntegral (min 255 ((baseVal + noise) * 160 `div` 210)) :: Word8
          b = fromIntegral (min 255 ((baseVal + noise) * 100 `div` 210)) :: Word8
      in (r, g, b, 255)

    -- Gravel: mix of gray pebble shapes
    gravelPattern x y =
      let n = pixHash x y 500 `mod` 100
          v = if n < 20 then 100 else if n < 50 then 130 else if n < 80 then 145 else 160
      in (v, v, v, 255)

    -- Oak log bark: brown vertical furrows with dark/light stripes
    logBark x y =
      let furrow = x `mod` 3
          noise = pixHash x y 600 `mod` 15
          stripe = pixHash (x `div` 2) y 601 `mod` 100
          isDark = furrow == 0 || stripe < 20
          (r, g, b) = if isDark
                      then (60 + fromIntegral noise, 40 + fromIntegral (noise `div` 2), 20 + fromIntegral (noise `div` 3))
                      else (105 + fromIntegral noise, 78 + fromIntegral (noise `div` 2), 42 + fromIntegral (noise `div` 3))
      in (r, g, b, 255)

    -- Leaves: transparent-ish green with holes
    leavesPattern x y =
      let n = pixHash x y 700 `mod` 100
          hole = n < 15
      in if hole then (20, 60, 15, 180) else (40 + fromIntegral (n `mod` 30), 120 + fromIntegral (n `mod` 40), 30, 255)

    -- Cobblestone: irregular rounded stone shapes with mortar gaps
    cobblePattern x y =
      let -- Block-level regions using ÷5 and ÷4 for irregular stone shapes
          bx        = x `div` 5
          by        = y `div` 4
          blockHash = pixHash bx by 800
          -- Stone tone varies per block region (90-150 range)
          stoneTone = 90 + blockHash `mod` 61
          -- Determine if this pixel is at a block boundary (mortar line)
          xInBlock  = x `mod` 5
          yInBlock  = y `mod` 4
          -- Offset alternate rows for irregular layout
          rowOffset = if by `mod` 2 == 0 then 0 else 2
          xShifted  = (x + rowOffset) `mod` 16
          bxShift   = xShifted `div` 5
          blockHashShift = pixHash bxShift by 800
          stoneShift = 90 + blockHashShift `mod` 61
          -- Mortar at boundaries with slight irregularity
          mortarJitter = pixHash x y 801 `mod` 3
          isMortarX = xInBlock == 0 || (xInBlock == 4 && mortarJitter == 0)
          isMortarY = yInBlock == 0 || (yInBlock == 3 && mortarJitter == 1)
          isMortar  = isMortarX || isMortarY
          -- Per-pixel noise for surface texture
          grain     = pixHash x y 802 `mod` 10
          -- Mortar color is darker (70-80 range)
          mortarV   = fromIntegral (70 + pixHash x y 803 `mod` 11)
          -- Use shifted block hash for final stone color to get irregular shapes
          rawStone  = if by `mod` 2 == 0 then stoneTone else stoneShift
          stoneV    = fromIntegral (min 155 (rawStone + grain))
      in if isMortar then (mortarV, mortarV, mortarV, 255)
         else (stoneV, stoneV, stoneV, 255)

    -- Bedrock: very dark gray (25-45) with irregular mineral inclusions
    bedrockPattern x y =
      let n = pixHash x y 900 `mod` 100
          inclusionN = pixHash (x + y * 13) (y + x * 7) 901 `mod` 100
          -- ~10% mineral inclusions
          isInclusion = inclusionN < 10
          -- Type of inclusion: brownish or lighter gray
          isBrownish = (inclusionN `mod` 2) == 0
          baseV = fromIntegral (25 + n `mod` 21) :: Word8  -- 25..45 range
      in if isInclusion
         then if isBrownish
              then (60, 50, 40, 255)   -- brownish speck
              else (75, 75, 80, 255)   -- lighter gray speck
         else (baseV, baseV, baseV, 255)

    -- Ore: stone base with colored spots
    orePattern x y (or', og, ob) =
      let isOreSpot = let n = pixHash (x `div` 3) (y `div` 3) 1000 `mod` 10
                      in n < 3 && (x + y) `mod` 3 /= 0
      in if isOreSpot then (or', og, ob, 255) else stonePattern x y

    -- Water: deep blue with caustic-like light patterns and surface ripples
    waterPattern x y =
      let -- Overlapping sine-based hash patterns for caustic highlights
          h1 = pixHash (x + 3) (y + 5) 1100 `mod` 256
          h2 = pixHash (x * 2 + 1) (y * 3 + 2) 1101 `mod` 256
          h3 = pixHash (x + y) (x - y + 16) 1102 `mod` 256
          caustic = (h1 + h2 + h3) `div` 3
          -- Bright caustic spots where multiple patterns overlap
          isBright = caustic > 180
          -- Surface ripple lines: horizontal lighter streaks every 4-5 pixels
          rippleRow = (y `mod` 5 == 0) || (y `mod` 5 == 1 && pixHash x y 1103 `mod` 3 == 0)
          -- Base deep blue with noise
          baseNoise = pixHash x y 1104 `mod` 20
          baseR = 20 + fromIntegral (baseNoise `div` 4)
          baseG = 60 + fromIntegral (baseNoise `div` 2)
          baseB = 180 + fromIntegral baseNoise
          -- Apply caustic brightening
          causticBoost = fromIntegral (caustic `div` 8)
          (cr, cg, cb) = if isBright
                         then ( min 255 (baseR + causticBoost + 30)
                              , min 255 (baseG + causticBoost + 25)
                              , min 255 (baseB + causticBoost + 15))
                         else (baseR + causticBoost `div` 3, baseG + causticBoost `div` 3, baseB + causticBoost `div` 4)
          -- Apply ripple brightening
          (rr, rg, rb) = if rippleRow
                         then (min 255 (cr + 15), min 255 (cg + 20), min 255 (cb + 10))
                         else (cr, cg, cb)
      in (rr, rg, rb, 200)

    -- Lava: bright orange-red base with dark crust and bright hotspot veins
    lavaPattern x y =
      let -- Per-pixel noise
          n = pixHash x y 1200 `mod` 100
          -- Large-scale crust pattern (divide by 5 for bigger blobs)
          crustHash = pixHash (x `div` 5) (y `div` 5) 1201 `mod` 100
          isCrust = crustHash < 35
          -- Bright hotspot veins between crust patches: thin lines, hash < threshold
          veinHash = pixHash (x + y `div` 2) (y + x `div` 3) 1202 `mod` 100
          isVein = not isCrust && veinHash < 8
          -- Base bright orange-red
          baseR = 230
          baseG = 100 + fromIntegral (n `mod` 30)
          baseB = 20 + fromIntegral (n `mod` 15)
          -- Dark crust patches
          crustR = 80 + fromIntegral (n `mod` 20)
          crustG = 30 + fromIntegral (n `mod` 15)
          crustB = 10 + fromIntegral (n `mod` 10)
          -- Bright yellow-white hotspot veins
          veinR = 255 :: Word8
          veinG = 230 + fromIntegral (n `mod` 25)
          veinB = 120 + fromIntegral (n `mod` 60)
          (r, g, b)
            | isVein  = (veinR, veinG, veinB)
            | isCrust = (crustR, crustG, crustB)
            | otherwise = (baseR, baseG, baseB)
      in (r, g, b, 255)

    -- Glass: thin gray border, mostly transparent interior with diagonal reflection streak
    glassPattern x y =
      let border = x == 0 || y == 0 || x == 15 || y == 15
          -- Diagonal reflection streak: 1-2px bright line from top-left toward center
          onStreak = let d = abs (x - y) in d <= 1 && x <= 8 && y <= 8
      in if border then (180, 200, 220, 150)
         else if onStreak then (240, 245, 255, 80)
         else (220, 235, 250, 35)

    -- Oak log cross-section: concentric rings from center (8,8) with bark edge
    logTop x y =
      let dx = x - 8
          dy = y - 8
          distSq = dx * dx + dy * dy
          dist = floor (sqrt (fromIntegral distSq :: Double)) :: Int
          ring = dist `div` 2
          ringParity = dist `mod` 2
          noise = pixHash x y 610 `mod` 8
      in if dist <= 1 then (55 + fromIntegral noise, 35 + fromIntegral noise, 18, 255)
         else if dist >= 7 then (100 + fromIntegral noise, 75 + fromIntegral (noise `div` 2), 40, 255)
         else if ringParity == 0
              then (170 + fromIntegral noise, 135 + fromIntegral noise, 70 + fromIntegral (noise `div` 2), 255)
              else (145 + fromIntegral noise, 110 + fromIntegral noise, 55 + fromIntegral (noise `div` 2), 255)

    -- Brick pattern: proper brick grid with alternating offset and per-brick color variation
    brickPattern x y =
      let row = y `div` 4
          offset = if row `mod` 2 == 0 then 0 else 8
          bx = (x + offset) `mod` 16
          -- Mortar lines: horizontal every 4 rows, vertical at brick boundaries
          isMortar = y `mod` 4 == 0 || bx `mod` 8 == 0
          -- Mortar color: gray with slight variation
          mortarN = pixHash x y 1300 `mod` 10
          mortarV = fromIntegral (155 + mortarN) :: Word8
          -- Brick identity for per-brick hash variation
          brickCol = (x + offset) `div` 8
          brickHash = pixHash brickCol row 1301
          -- Base red tone with per-brick variation (±15 per channel)
          baseR = 150 + brickHash `mod` 41           -- 150-190
          baseG = 50 + (brickHash `div` 41) `mod` 21 -- 50-70
          baseB = 40 + (brickHash `div` 861) `mod` 11 -- 40-50
          -- Per-brick variation offset (±15)
          varSeed = pixHash brickCol row 1302
          varR = (varSeed `mod` 31) - 15
          varG = ((varSeed `div` 31) `mod` 31) - 15
          varB = ((varSeed `div` 961) `mod` 31) - 15
          -- Subtle per-pixel noise within each brick (±5)
          pixN = pixHash x y 1303 `mod` 11 - 5
          r = clampByte (baseR + varR + pixN)
          g = clampByte (baseG + varG + pixN)
          b = clampByte (baseB + varB + pixN)
      in if isMortar
         then (mortarV, mortarV, mortarV, 255)
         else (fromIntegral r, fromIntegral g, fromIntegral b, 255)

    -- TNT side: red middle band with white bands top/bottom + dark stripes
    tntSide x y =
      let -- Dark border rows at edges of red band
          isBorderRow = y == 4 || y == 11
          -- Dark horizontal stripe through middle of red band
          isDarkStripe = y == 7 || y == 8
          -- "TNT" letter shapes on the red band (y 5-10, centered)
          -- T: x 1-5, top bar y=5,6; stem x=2,3 y=7-10
          isLetterT = (x >= 1 && x <= 5 && (y == 5 || y == 6))
                   || (x >= 2 && x <= 3 && y >= 7 && y <= 10)
          -- N: x 6-9; left stem x=6, right stem x=9, diag
          isLetterN = (x == 6 && y >= 5 && y <= 10)
                   || (x == 9 && y >= 5 && y <= 10)
                   || (x == 7 && (y == 6 || y == 7))
                   || (x == 8 && (y == 8 || y == 9))
          -- T2: x 10-14, top bar y=5,6; stem x=11,12 y=7-10
          isLetterT2 = (x >= 10 && x <= 14 && (y == 5 || y == 6))
                    || (x >= 11 && x <= 12 && y >= 7 && y <= 10)
          isLetter = isLetterT || isLetterN || isLetterT2
          -- White bands: top y 0-3, bottom y 12-15
          isWhiteBand = y <= 3 || y >= 12
          n = pixHash x y 850 `mod` 10
      in if isWhiteBand
         then let v = 220 + fromIntegral n * 3 in (v, v, v, 255)
         else if isBorderRow
         then (140, 25, 25, 255)
         else if isLetter
         then (50, 15, 15, 255)
         else if isDarkStripe
         then (160, 30, 30, 255)
         else (200, 40, 40, 255)

    -- TNT top: tan/beige base with dark fuse circle in center
    tntTop x y =
      let dx = x - 7
          dy = y - 7
          distSq = dx * dx + dy * dy
          -- Fuse hole: small dark circle radius ~2
          isFuse = distSq <= 4
          -- Fuse ring: slightly larger
          isFuseRing = distSq <= 9 && distSq > 4
          n = pixHash x y 860 `mod` 15
          -- Tan base with subtle grain
          baseR = 200 + fromIntegral n
          baseG = 180 + fromIntegral n
          baseB = 130 + fromIntegral (n `div` 2)
      in if isFuse then (40, 35, 30, 255)
         else if isFuseRing then (120, 100, 70, 255)
         else (baseR, baseG, baseB, 255)

    -- TNT bottom: flat tan surface
    tntBottom x y =
      let n = pixHash x y 870 `mod` 12
          r = 195 + fromIntegral n
          g = 175 + fromIntegral n
          b = 125 + fromIntegral (n `div` 2)
      in (r, g, b, 255)

    -- Chest: brown wooden body with horizontal plank grain, gold latch, dark border
    chestPattern x y =
      let border = x == 0 || y == 0 || x == 15 || y == 15
          topBorder = y <= 1
          bottomBorder = y >= 14
          latch = x >= 6 && x <= 9 && y >= 7 && y <= 8
          -- Horizontal plank grain
          n = pixHash x y 1400 `mod` 15
          grain = pixHash x (y `div` 3) 1401 `mod` 10
          plankLine = y `mod` 4 == 0
          baseR = if plankLine then fromIntegral (130 + n) else fromIntegral (145 + n)
          baseG = if plankLine then fromIntegral (85 + n) else fromIntegral (95 + n)
          baseB = if plankLine then fromIntegral (35 + grain) else fromIntegral (45 + grain)
      in if latch then (220, 190, 50, 255)
         else if topBorder || bottomBorder then (60, 40, 20, 255)
         else if border then (80, 55, 25, 255)
         else (baseR, baseG, baseB, 255)

    -- Furnace side: stone-gray casing with dark furnace opening in center
    furnaceSide x y =
      let opening = x >= 4 && x <= 11 && y >= 5 && y <= 12
          n = pixHash x y 1300 `mod` 12
          -- Stone-gray casing base (130,130,130) with noise
          casingV = fromIntegral (124 + n) :: Word8
      in if opening then (40, 40, 40, 255)
         else (casingV, casingV, casingV, 255)

    -- Furnace top: gray stone top with subtle grid pattern
    furnaceTop x y =
      let n = pixHash x y 1310 `mod` 10
          gridLine = x `mod` 4 == 0 || y `mod` 4 == 0
          baseV = if gridLine then fromIntegral (118 + n) else fromIntegral (128 + n)
      in (baseV, baseV, baseV, 255)

    -- Stone brick: clean 8x8 grid with mortar lines and chisel mark texture
    stoneBrickPattern x y =
      let offset = if (y `div` 8) `mod` 2 == 0 then 0 else 4
          bx = (x + offset) `mod` 8
          -- 1px gray mortar lines at boundaries
          isMortar = y `mod` 8 == 0 || bx == 0
          mortarN = pixHash x y 1500 `mod` 11
          mortarV = fromIntegral (100 + mortarN) :: Word8  -- 100-110
          -- Brick identity for per-brick base tone
          brickCol = (x + offset) `div` 8
          brickRow = y `div` 8
          brickHash = pixHash brickCol brickRow 1501
          baseV = 130 + brickHash `mod` 21  -- 130-150 smooth gray
          -- Subtle chisel marks: occasional darker diagonal pixels
          chiselSeed = pixHash x y 1502
          isDiag = (x + y) `mod` 5 == 0 || (x - y + 16) `mod` 7 == 0
          isChisel = isDiag && chiselSeed `mod` 8 < 2
          chiselDark = -15
          -- Per-pixel subtle noise (±3)
          pixN = pixHash x y 1503 `mod` 7 - 3
          v = clampByte (baseV + pixN + if isChisel then chiselDark else 0)
      in if isMortar
         then (mortarV, mortarV, mortarV, 255)
         else (fromIntegral v, fromIntegral v, fromIntegral v, 255)

    -- Crafting table top: brown wooden 4x4 grid with per-cell tone and tool marks
    craftingTop x y =
      let gridLine = x `mod` 4 == 0 || y `mod` 4 == 0
          cellX = x `div` 4
          cellY = y `div` 4
          cellHash = pixHash cellX cellY 1500 `mod` 20
          -- Per-cell tone variation on tan/brown base
          baseR = fromIntegral (160 + cellHash) :: Word8
          baseG = fromIntegral (120 + cellHash) :: Word8
          baseB = fromIntegral (70 + cellHash `div` 2) :: Word8
          -- Occasional darker diagonal scratches (tool marks)
          scratch = pixHash (x + y) (x - y + 16) 1501 `mod` 30 < 2
          scratchR = fromIntegral (max 0 (fromIntegral baseR - 40 :: Int)) :: Word8
          scratchG = fromIntegral (max 0 (fromIntegral baseG - 30 :: Int)) :: Word8
          scratchB = fromIntegral (max 0 (fromIntegral baseB - 20 :: Int)) :: Word8
      in if gridLine then (80, 55, 25, 255)
         else if scratch then (scratchR, scratchG, scratchB, 255)
         else (baseR, baseG, baseB, 255)

    -- Snow: white with subtle variation
    snowPattern x y =
      let n = pixHash x y 1600 `mod` 15
          v = fromIntegral (240 + n)
      in (v, v, v, 255)

    -- Clay: light gray-brown
    clayPattern x y =
      let n = pixHash x y 1700 `mod` 20
          v = fromIntegral (160 + n)
      in (v, fromIntegral (155 + n), fromIntegral (145 + n), 255)

    -- Torch: brown stick with shaped flame using distance falloff
    torchPattern x y =
      let isStick = x >= 7 && x <= 8 && y >= 6 && y <= 15
          -- Flame region at top (y 0-5), shaped by distance from center
          cx = 7.5 :: Double
          cy = 2.5 :: Double
          dx = fromIntegral x - cx
          dy = fromIntegral y - cy
          dist = sqrt (dx * dx + dy * dy) :: Double
          isFlame = y <= 5 && dist < 4.0
          -- Interpolate bright yellow center to orange at edges
          t = min 1.0 (dist / 4.0) :: Double
          flameR = round (255.0 - t * 0.0) :: Word8      -- stays 255
          flameG = round (220.0 - t * 70.0) :: Word8     -- 220 -> 150
          flameB = round (50.0 - t * 20.0) :: Word8      -- 50 -> 30
      in if isFlame then (flameR, flameG, flameB, 255)
         else if isStick then (100, 70, 35, 255)
         else (0, 0, 0, 0)  -- transparent

    -- Obsidian: deep purple-black base with glassy reflection spots and purple streaks
    obsidianPattern x y =
      let n = pixHash x y 2000 `mod` 100
          -- Glassy reflection spots: ~8% of pixels slightly brighter
          isReflection = n < 8
          -- Purple highlight streak: diagonal bands
          streakN = pixHash (x + y `div` 2) (y `div` 3) 2001 `mod` 20
          isStreak = streakN < 2
          -- Subtle variation in base color
          vari = pixHash (x * 3 + 1) (y * 7 + 2) 2002 `mod` 8
          (r, g, b)
            | isReflection = (40, 20, 50)
            | isStreak     = (45, 15, 60)
            | n < 40       = (20 + fromIntegral vari, 10 + fromIntegral (vari `div` 2), 30 + fromIntegral vari)
            | otherwise    = (20, 10, 30)
      in (r, g, b, 255)

    -- Oak door closed: brown planks with border and doorknob
    oakDoorClosedPattern x y =
      let border = x == 0 || x == 15 || y == 0 || y == 15
          knob = x >= 11 && x <= 13 && y >= 7 && y <= 9
          plank = y `mod` 4 == 0
      in if knob then (180, 160, 40, 255)
         else if border then (80, 55, 25, 255)
         else if plank then (120, 85, 40, 255)
         else (150, 110, 55, 255)

    -- Oak door open: thin brown edge (door seen from side)
    oakDoorOpenPattern x y =
      let isDoor = x >= 6 && x <= 9
      in if isDoor then (140, 100, 50, 255)
         else (0, 0, 0, 0)  -- transparent

    -- Ladder: brown cross-hatch pattern (rungs)
    ladderPattern x y =
      let isRail = x == 2 || x == 13
          isRung = y `mod` 4 == 0 && x >= 2 && x <= 13
      in if isRail || isRung then (120, 85, 40, 255)
         else (0, 0, 0, 0)  -- transparent

    -- Bed: red top with white pillow area
    bedPattern x y =
      let isPillow = y >= 1 && y <= 5 && x >= 2 && x <= 13
          border = x == 0 || x == 15 || y == 0 || y == 15
      in if border then (100, 60, 30, 255)
         else if isPillow then (220, 220, 220, 255)
         else (180, 40, 40, 255)

    -- Oak fence: brown vertical post with cross rails
    oakFencePattern x y =
      let isPost = x >= 6 && x <= 9
          isRail = (y >= 3 && y <= 5) || (y >= 10 && y <= 12)
      in if isPost then (130, 95, 45, 255)
         else if isRail then (120, 85, 40, 255)
         else (0, 0, 0, 0)  -- transparent

    -- Farmland: dark brown rows (tilled earth)
    farmlandPattern x y =
      let row = y `mod` 4 < 2
          n = pixHash x y 2100 `mod` 20
          v = if row then 70 + n else 55 + n
      in (fromIntegral v, fromIntegral (v * 45 `div` 70), fromIntegral (v * 25 `div` 70), 255)

    -- Wheat crop: green/yellow stalks on transparent background
    wheatCropPattern x y =
      let isStalk = x `mod` 3 == 1 && y >= 3
          n = pixHash x y 2200 `mod` 50
          green = (80 + fromIntegral n, 140 + fromIntegral (n `div` 2), 20, 255)
          gold = (180 + fromIntegral (n `div` 3), 160, 30, 255)
          tip = y <= 5
      in if isStalk then (if tip then gold else green)
         else (0, 0, 0, 0)

    -- Oak sapling: thin brown trunk with small green canopy on transparent bg
    oakSaplingPattern x y =
      let -- Thin trunk: x=7-8, y=10-15
          isTrunk = (x == 7 || x == 8) && y >= 10 && y <= 15
          -- Small green canopy: 3x3 block at y=6-8, x=6-8 plus 1px border
          isCanopy = x >= 6 && x <= 9 && y >= 6 && y <= 9
                     && not (x == 6 && y == 6) && not (x == 9 && y == 6)
                     && not (x == 6 && y == 9) && not (x == 9 && y == 9)
          n = pixHash x y 2300 `mod` 30
      in if isCanopy then (30 + fromIntegral n, 120 + fromIntegral n, 20, 255)
         else if isTrunk then (100, 70, 30, 255)
         else (0, 0, 0, 0)

    -- Wool: white with subtle fiber pattern (light gray lines on white)
    woolPattern x y =
      let n = pixHash x y 2400 `mod` 100
          fiber = pixHash (x `div` 2) (y + x `div` 3) 2401 `mod` 10 < 2
          v = if fiber then 210 + fromIntegral (n `mod` 15)
              else 235 + fromIntegral (n `mod` 20)
      in (v, v, v, 255)

    -- Fence gate closed: brown planks with cross-brace pattern
    fenceGateClosedPattern x y =
      let isPost = (x >= 1 && x <= 3) || (x >= 12 && x <= 14)
          isRail = (y >= 3 && y <= 5) || (y >= 10 && y <= 12)
          isCrossBrace = x >= 4 && x <= 11 && (isRail || y == 7 || y == 8)
          border = x == 0 || x == 15
      in if border then (80, 55, 25, 255)
         else if isPost then (130, 95, 45, 255)
         else if isCrossBrace then (120, 85, 40, 255)
         else (0, 0, 0, 0)

    -- Fence gate open: thin side edges only (gate swung open)
    fenceGateOpenPattern x y =
      let isPost = (x >= 1 && x <= 3) || (x >= 12 && x <= 14)
          isRail = (y >= 3 && y <= 5) || (y >= 10 && y <= 12)
      in if isPost && isRail then (120, 85, 40, 255)
         else if isPost then (130, 95, 45, 255)
         else (0, 0, 0, 0)

    -- Lever: gray stick on stone background
    leverPattern x y =
      let isStick = x >= 7 && x <= 8 && y >= 4 && y <= 13
          isBase  = x >= 5 && x <= 10 && y >= 13 && y <= 15
      in if isStick then (120, 120, 120, 255)
         else if isBase then stonePattern x y
         else (0, 0, 0, 0)

    -- Redstone dust: red cross pattern (powered)
    redstoneDustPattern x y =
      let isCross = (x >= 6 && x <= 9) || (y >= 6 && y <= 9)
          isCenter = x >= 6 && x <= 9 && y >= 6 && y <= 9
      in if isCenter then (200, 0, 0, 255)
         else if isCross then (140, 0, 0, 255)
         else (0, 0, 0, 0)

    -- Trapdoor closed: brown planks with grid lines (flat hatch)
    trapdoorClosedPattern x y =
      let border = x == 0 || x == 15 || y == 0 || y == 15
          gridLine = x == 7 || x == 8 || y == 7 || y == 8
          n = pixHash x y 2500 `mod` 20
          plankR = 140 + fromIntegral n
          plankG = plankR * 75 `div` 140
          plankB = plankR * 45 `div` 140
      in if border then (80, 55, 25, 255)
         else if gridLine then (100, 70, 35, 255)
         else (plankR, plankG, plankB, 255)

    -- Trapdoor open: thin horizontal edge (trapdoor seen from side, open position)
    trapdoorOpenPattern x y =
      let isTrapdoor = y <= 3
          gridLine = y == 0 || x == 7 || x == 8
          n = pixHash x y 2600 `mod` 15
      in if isTrapdoor then
           if gridLine then (100, 70, 35, 255)
           else (130 + fromIntegral n, 90 + fromIntegral (n `div` 2), 40, 255)
         else (0, 0, 0, 0)  -- transparent

    -- Iron door closed: gray metallic surface with rivets and border
    ironDoorClosedPattern x y =
      let border = x == 0 || x == 15 || y == 0 || y == 15
          knob = x >= 11 && x <= 13 && y >= 7 && y <= 9
          rivet = (x == 3 || x == 12) && (y == 3 || y == 12)
          n = pixHash x y 2500 `mod` 20
          base = fromIntegral (160 + n) :: Word8
          dark = fromIntegral (120 + n) :: Word8
      in if knob then (100, 100, 110, 255)
         else if rivet then (200, 200, 210, 255)
         else if border then (dark, dark, fromIntegral (dark + 5), 255)
         else (base, base, fromIntegral (base + 5), 255)

    -- Iron door open: thin gray edge (door seen from side)
    ironDoorOpenPattern x y =
      let isDoor = x >= 6 && x <= 9
          n = pixHash x y 2510 `mod` 15
          v = fromIntegral (160 + n) :: Word8
      in if isDoor then (v, v, fromIntegral (v + 5), 255)
         else (0, 0, 0, 0)  -- transparent

    -- Fire: orange/yellow flickering flame pattern
    firePattern x y =
      let n = pixHash x y 2500 `mod` 100
          -- Flame shape: wider at bottom, narrower at top
          centerDist = abs (x - 7)
          maxWidth = max 1 (8 - y `div` 2)
          isFlame = centerDist <= maxWidth && y >= 2
          isTip = y <= 4 && centerDist <= 2
          isBase = y >= 12 && centerDist <= 6
      in if not isFlame && not isBase then (0, 0, 0, 0)  -- transparent
         else if isTip then (255, 255, fromIntegral (100 + n `mod` 80), 255)  -- bright yellow tip
         else if y <= 6 then (255, fromIntegral (180 + n `mod` 60), fromIntegral (20 + n `mod` 30), 255)  -- orange-yellow upper
         else if y <= 10 then (255, fromIntegral (120 + n `mod` 60), fromIntegral (10 + n `mod` 20), 255)  -- orange middle
         else (220, fromIntegral (80 + n `mod` 40), 10, 255)  -- dark orange base

    -- Cactus: green body with darker vertical stripes and small thorns
    cactusPattern x y =
      let n = pixHash x y 2500 `mod` 100
          -- Vertical stripes every 4 pixels
          stripe = x `mod` 4 == 0
          -- Thorns: small dots scattered on the surface
          thorn = (x + y * 3) `mod` 7 == 0 && n < 40
          -- Border (slightly inset to show it's narrower than a full block)
          border = x == 0 || x == 15 || y == 0 || y == 15
          (r, g, b)
            | border    = (20, 80, 15)
            | thorn     = (140, 160, 80)
            | stripe    = (25, 100, 20)
            | n < 30    = (35, 130, 25)
            | n < 60    = (45, 150, 35)
            | otherwise = (40, 140, 30)
      in (r, g, b, 255)

    -- Sugar cane: green vertical reeds on transparent background
    sugarCanePattern x y =
      let isReed1 = x >= 2 && x <= 4
          isReed2 = x >= 7 && x <= 9
          isReed3 = x >= 12 && x <= 13
          isReed = isReed1 || isReed2 || isReed3
          isNode = isReed && (y `mod` 5 == 0)
          n = pixHash x y 2500 `mod` 40
          (gr, gg, gb) = if isNode then (60, 130 + fromIntegral n, 40)
                         else (70 + fromIntegral (n `div` 2), 150 + fromIntegral n, 50)
      in if isReed then (gr, gg, gb, 255)
         else (0, 0, 0, 0)

    -- Piston top: stone face with a slight cross pattern
    pistonTopPattern x y =
      let n = pixHash x y 2700 `mod` 100
          border = x == 0 || x == 15 || y == 0 || y == 15
          cross = (x >= 7 && x <= 8) || (y >= 7 && y <= 8)
          v = if border then 90
              else if cross then fromIntegral (110 + n `mod` 15)
              else fromIntegral (120 + n `mod` 20)
      in (v, v, v, 255)

    -- Piston side: wooden planks with iron band
    pistonSidePattern x y =
      let n = pixHash x y 2710 `mod` 30
          ironBand = y >= 6 && y <= 9
          border = x == 0 || x == 15 || y == 0 || y == 15
          (r, g, b)
            | border    = (80, 55, 25)
            | ironBand  = (140 + fromIntegral n, 140 + fromIntegral n, 150 + fromIntegral n)
            | otherwise = let base = 160 + fromIntegral n
                          in (base, base * 170 `div` 210, base * 110 `div` 210)
      in (r, g, b, 255)

    -- Piston head: flat wooden panel with border
    pistonHeadPattern x y =
      let n = pixHash x y 2720 `mod` 20
          border = x == 0 || x == 15 || y == 0 || y == 15
          grain = y `mod` 4 == 0
          (r, g, b)
            | border = (80, 55, 25)
            | grain  = (150 + fromIntegral n, fromIntegral ((150 + n) * 170 `div` 210), fromIntegral ((150 + n) * 110 `div` 210))
            | otherwise = let base = 180 + fromIntegral n
                          in (base, base * 170 `div` 210, base * 110 `div` 210)
      in (r, g, b, 255)

    -- Rail: gray parallel rails on brown wooden ties
    railPattern x y =
      let isLeftRail  = x >= 2 && x <= 4
          isRightRail = x >= 11 && x <= 13
          isRail      = isLeftRail || isRightRail
          isTie       = y `mod` 4 >= 1 && y `mod` 4 <= 2 && x >= 1 && x <= 14
          n = pixHash x y 2700 `mod` 20
      in if isRail then (140 + fromIntegral n, 140 + fromIntegral n, 145 + fromIntegral n, 255)
         else if isTie then (90 + fromIntegral (n `div` 2), 60 + fromIntegral (n `div` 3), 30, 255)
         else (0, 0, 0, 0)
    -- Dispenser front: stone gray base with dark centered face/mouth opening
    dispenserFrontPattern x y =
      let -- Distinctive mouth shape at y=8-12
          isMouth = x >= 4 && x <= 11 && y >= 8 && y <= 12
          -- Upper face opening (eyes area)
          isEye = (x >= 4 && x <= 6 && y >= 5 && y <= 7)
               || (x >= 9 && x <= 11 && y >= 5 && y <= 7)
          border = x == 0 || x == 15 || y == 0 || y == 15
          n = pixHash x y 2700 `mod` 20
          baseV = fromIntegral (120 + n) :: Word8
      in if isMouth then (25, 25, 30, 255)
         else if isEye then (35, 35, 40, 255)
         else if border then (90, 90, 95, 255)
         else (baseV, baseV, fromIntegral (baseV + 3), 255)

    -- Enchanting table top: dark purple-black surface with glowing blue-cyan rune symbols
    enchantingTableTop x y =
      let n = pixHash x y 2700 `mod` 100
          -- Glowing rune-like symbols: simple geometric shapes in blue-cyan
          isRuneCircle = let dx = x - 7; dy = y - 7
                             dist = dx * dx + dy * dy
                         in dist >= 9 && dist <= 16
          isRuneCross = ((x == 7 || x == 8) && y >= 3 && y <= 12)
                     || ((y == 7 || y == 8) && x >= 3 && x <= 12)
          isRuneCorner = (x == 3 && y == 3) || (x == 12 && y == 3)
                      || (x == 3 && y == 12) || (x == 12 && y == 12)
          isRuneDiamond = (x == 7 && y == 2) || (x == 8 && y == 2)
                       || (x == 7 && y == 13) || (x == 8 && y == 13)
                       || (x == 2 && y == 7) || (x == 2 && y == 8)
                       || (x == 13 && y == 7) || (x == 13 && y == 8)
          isRune = isRuneCircle || isRuneCorner || isRuneDiamond
          isRuneFaint = isRuneCross && not isRuneCircle
          border = x == 0 || x == 15 || y == 0 || y == 15
          -- Base surface noise
          baseR = fromIntegral (28 + n `mod` 6) :: Word8
          baseG = fromIntegral (13 + n `mod` 5) :: Word8
          baseB = fromIntegral (38 + n `mod` 6) :: Word8
      in if isRune then (80, 200, 240, 255)       -- bright blue-cyan rune glow
         else if isRuneFaint then (40, 100, 130, 255) -- subtle inner rune lines
         else if border then (20, 8, 28, 255)
         else (baseR, baseG, baseB, 255)

    -- Enchanting table side: dark wood/obsidian base with subtle purple trim at top edge
    enchantingTableSide x y =
      let n = pixHash x y 2800 `mod` 100
          -- Purple trim at top edge (rows 0-2)
          purpleTrim = y <= 2
          border = x == 0 || x == 15 || y == 0 || y == 15
          -- Dark wood/obsidian body with subtle variation
          streak = pixHash (x `div` 3) (y `div` 2) 2801 `mod` 10 < 2
          (r, g, b)
            | purpleTrim && border = (50, 20, 70)
            | purpleTrim           = (60 + fromIntegral (n `mod` 15), 25 + fromIntegral (n `mod` 8), 80 + fromIntegral (n `mod` 15))
            | border               = (15, 8, 20)
            | streak               = (30, 15, 40)
            | n < 30               = (25, 14, 30)
            | otherwise            = (32, 18, 36)
      in (r, g, b, 255)

    -- Netherrack: dark red with brownish veins
    netherrackPattern x y =
      let n = pixHash x y 3000 `mod` 100
          vein = pixHash (x `div` 3) (y `div` 2) 3001 `mod` 10 < 2
          (r, g, b) = if vein then (100, 40, 35) else if n < 30 then (110, 50, 45) else (120, 55, 50)
      in (r, g, b, 255)

    -- Soul sand: dark brown with wailing-face-like dark spots
    soulSandPattern x y =
      let n = pixHash x y 3100 `mod` 100
          face = pixHash (x `div` 4) (y `div` 4) 3101 `mod` 10 < 3
          (r, g, b) = if face then (60, 45, 30) else if n < 40 then (80, 60, 40) else (90, 70, 50)
      in (r, g, b, 255)

    -- Glowstone: bright yellow-gold with luminous cracks
    glowstonePattern x y =
      let n = pixHash x y 3200 `mod` 100
          bright = pixHash (x `div` 2) (y `div` 2) 3201 `mod` 10 < 3
          (r, g, b) = if bright then (255, 230, 120) else if n < 30 then (200, 170, 80) else (220, 190, 100)
      in (r, g, b, 255)

    -- Nether brick: dark red-brown bricks with mortar lines
    netherBrickPattern x y =
      let row = y `div` 4
          offset = if row `mod` 2 == 0 then 0 else 8
          bx = (x + offset) `mod` 16
          isMortar = y `mod` 4 == 0 || bx `mod` 8 == 0
      in if isMortar then (30, 20, 20, 255) else (70, 30, 30, 255)

    -- Nether portal: swirling purple pattern (semi-transparent)
    netherPortalPattern x y =
      let n = pixHash x y 3300 `mod` 100
          swirl = pixHash (x + y `div` 2) (y `div` 3) 3301 `mod` 10 < 4
          (r, g, b, a) = if swirl then (140, 50, 200, 200) else (80 + fromIntegral (n `mod` 30), 20, 130 + fromIntegral (n `mod` 40), 160)
      in (r, g, b, a)

    -- Mossy cobblestone: cobblestone with green moss patches
    mossyCobblePattern x y =
      let base@(br, bg, bb, _) = cobblePattern x y
          mossy = pixHash (x `div` 3) (y `div` 3) 3400 `mod` 10 < 4
      in if mossy then (fromIntegral (br `div` 2), fromIntegral (min 255 (fromIntegral bg + 40)), fromIntegral (bb `div` 2), 255) else base

    -- Mossy stone brick: stone bricks with green moss in mortar
    mossyStoneBrickPattern x y =
      let isMortar = y `mod` 8 == 0 || (x + (if (y `div` 8) `mod` 2 == 0 then 0 else 4)) `mod` 8 == 0
          n = pixHash x y 3500 `mod` 20
          mossy = pixHash (x `div` 4) (y `div` 4) 3501 `mod` 10 < 4
      in if isMortar && mossy then (60, fromIntegral (120 + n), 40, 255)
         else if isMortar then (100, fromIntegral (100 + n), 100, 255)
         else let v = fromIntegral (130 + n) in (v, v, v, 255)

    -- Birch bark: white/cream with dark horizontal marks
    birchBark x y =
      let noise = pixHash x y 4000 `mod` 12
          -- Dark horizontal marks characteristic of birch
          hasMark = (pixHash 0 (y `div` 2) 4001 `mod` 5 < 2) && (pixHash x y 4002 `mod` 3 < 2)
          isVFurrow = x `mod` 4 == 0 && pixHash x y 4003 `mod` 3 == 0
          (r, g, b)
            | hasMark   = (45 + fromIntegral noise, 40 + fromIntegral noise, 35 + fromIntegral noise)
            | isVFurrow = (185 + fromIntegral noise, 180 + fromIntegral noise, 170 + fromIntegral noise)
            | otherwise = (215 + fromIntegral noise, 210 + fromIntegral noise, 200 + fromIntegral noise)
      in (r, g, b, 255)

    -- Birch log cross-section: concentric rings, light cream wood, bark-colored edge
    birchLogTop x y =
      let dx = x - 8
          dy = y - 8
          distSq = dx * dx + dy * dy
          dist = floor (sqrt (fromIntegral distSq :: Double)) :: Int
          ringParity = dist `mod` 2
          noise = pixHash x y 4110 `mod` 8
      in if dist <= 1 then (160 + fromIntegral noise, 140 + fromIntegral noise, 100 + fromIntegral noise, 255)
         else if dist >= 7 then (210 + fromIntegral noise, 205 + fromIntegral noise, 195 + fromIntegral noise, 255)
         else if ringParity == 0
              then (220 + fromIntegral noise, 210 + fromIntegral noise, 170 + fromIntegral (noise `div` 2), 255)
              else (200 + fromIntegral noise, 190 + fromIntegral noise, 150 + fromIntegral (noise `div` 2), 255)

    -- Birch leaves: lighter green with yellowish tint
    birchLeavesPattern x y =
      let n = pixHash x y 4100 `mod` 100
          hole = n < 15
      in if hole then (30, 70, 20, 180)
         else (60 + fromIntegral (n `mod` 30), 140 + fromIntegral (n `mod` 30), 40 + fromIntegral (n `mod` 20), 255)

    -- Birch planks: light cream, 4 horizontal plank divisions with grain
    birchPlanksPattern x y =
      let plankIdx = y `div` 4
          plankVar = pixHash 0 plankIdx 4200 `mod` 15
          grainLine = y `mod` 4 == 0
          grain = pixHash x y 4201 `mod` 100
          noise = pixHash x y 4202 `mod` 8
          baseVal = if grainLine then 190 + plankVar
                    else 215 + plankVar + (grain `div` 25)
          r = fromIntegral (min 255 (baseVal + noise)) :: Word8
          g = fromIntegral (min 255 ((baseVal + noise) * 200 `div` 230)) :: Word8
          b = fromIntegral (min 255 ((baseVal + noise) * 165 `div` 230)) :: Word8
      in (r, g, b, 255)

    -- Spruce bark: very dark brown, vertical furrows
    spruceBark x y =
      let furrow = x `mod` 3
          noise = pixHash x y 4300 `mod` 10
          stripe = pixHash (x `div` 2) y 4301 `mod` 100
          isDark = furrow == 0 || stripe < 20
          (r, g, b) = if isDark
                      then (30 + fromIntegral noise, 20 + fromIntegral (noise `div` 2), 10 + fromIntegral (noise `div` 3))
                      else (55 + fromIntegral noise, 40 + fromIntegral (noise `div` 2), 22 + fromIntegral (noise `div` 3))
      in (r, g, b, 255)

    -- Spruce log cross-section: concentric rings, dark wood, bark-colored edge
    spruceLogTop x y =
      let dx = x - 8
          dy = y - 8
          distSq = dx * dx + dy * dy
          dist = floor (sqrt (fromIntegral distSq :: Double)) :: Int
          ringParity = dist `mod` 2
          noise = pixHash x y 4310 `mod` 8
      in if dist <= 1 then (40 + fromIntegral noise, 28 + fromIntegral noise, 15, 255)
         else if dist >= 7 then (55 + fromIntegral noise, 40 + fromIntegral (noise `div` 2), 22, 255)
         else if ringParity == 0
              then (130 + fromIntegral noise, 95 + fromIntegral noise, 50 + fromIntegral (noise `div` 2), 255)
              else (105 + fromIntegral noise, 75 + fromIntegral noise, 38 + fromIntegral (noise `div` 2), 255)

    -- Spruce leaves: dark blue-green
    spruceLeavesPattern x y =
      let n = pixHash x y 4400 `mod` 100
          hole = n < 12
      in if hole then (10, 30, 15, 180)
         else (20 + fromIntegral (n `mod` 20), 60 + fromIntegral (n `mod` 30), 25 + fromIntegral (n `mod` 15), 255)

    -- Spruce planks: dark brown, 4 horizontal plank divisions with grain
    sprucePlanksPattern x y =
      let plankIdx = y `div` 4
          plankVar = pixHash 0 plankIdx 4500 `mod` 15
          grainLine = y `mod` 4 == 0
          grain = pixHash x y 4501 `mod` 100
          noise = pixHash x y 4502 `mod` 8
          baseVal = if grainLine then 100 + plankVar
                    else 125 + plankVar + (grain `div` 25)
          r = fromIntegral (min 255 (baseVal + noise)) :: Word8
          g = fromIntegral (min 255 ((baseVal + noise) * 80 `div` 140)) :: Word8
          b = fromIntegral (min 255 ((baseVal + noise) * 50 `div` 140)) :: Word8
      in (r, g, b, 255)

    -- Jungle bark: tan with vertical furrows and vine marks
    jungleBark x y =
      let furrow = x `mod` 4
          noise = pixHash x y 4600 `mod` 12
          stripe = pixHash (x `div` 2) y 4601 `mod` 100
          isDark = furrow == 0 || stripe < 15
          -- Vine marks: scattered green-tinted spots
          isVine = pixHash x y 4602 `mod` 11 == 0
          (r, g, b)
            | isVine    = (60 + fromIntegral noise, 85 + fromIntegral noise, 35 + fromIntegral (noise `div` 2))
            | isDark    = (75 + fromIntegral noise, 58 + fromIntegral (noise `div` 2), 30 + fromIntegral (noise `div` 3))
            | otherwise = (130 + fromIntegral noise, 105 + fromIntegral (noise `div` 2), 55 + fromIntegral (noise `div` 3))
      in (r, g, b, 255)

    -- Jungle log cross-section: concentric rings, tan-orange wood, bark-colored edge
    jungleLogTop x y =
      let dx = x - 8
          dy = y - 8
          distSq = dx * dx + dy * dy
          dist = floor (sqrt (fromIntegral distSq :: Double)) :: Int
          ringParity = dist `mod` 2
          noise = pixHash x y 4610 `mod` 8
      in if dist <= 1 then (75 + fromIntegral noise, 55 + fromIntegral noise, 28, 255)
         else if dist >= 7 then (130 + fromIntegral noise, 105 + fromIntegral (noise `div` 2), 55, 255)
         else if ringParity == 0
              then (165 + fromIntegral noise, 135 + fromIntegral noise, 70 + fromIntegral (noise `div` 2), 255)
              else (140 + fromIntegral noise, 112 + fromIntegral noise, 55 + fromIntegral (noise `div` 2), 255)

    -- Jungle leaves: vibrant green
    jungleLeavesPattern x y =
      let n = pixHash x y 4700 `mod` 100
          hole = n < 10
      in if hole then (15, 50, 10, 180)
         else (30 + fromIntegral (n `mod` 25), 100 + fromIntegral (n `mod` 40), 20 + fromIntegral (n `mod` 15), 255)

    -- Jungle planks: tan-orange, 4 horizontal plank divisions with grain
    junglePlanksPattern x y =
      let plankIdx = y `div` 4
          plankVar = pixHash 0 plankIdx 4800 `mod` 18
          grainLine = y `mod` 4 == 0
          grain = pixHash x y 4801 `mod` 100
          noise = pixHash x y 4802 `mod` 8
          baseVal = if grainLine then 145 + plankVar
                    else 170 + plankVar + (grain `div` 25)
          r = fromIntegral (min 255 (baseVal + noise)) :: Word8
          g = fromIntegral (min 255 ((baseVal + noise) * 120 `div` 190)) :: Word8
          b = fromIntegral (min 255 ((baseVal + noise) * 70 `div` 190)) :: Word8
      in (r, g, b, 255)

    -- Tall grass: 2-3 distinct green blade shapes rising from bottom on transparent bg
    tallGrassPattern x y =
      let -- Blade 1 at x=3, 2px wide, height 12 (y=4..15), darker green
          blade1 = (x == 3 || x == 4) && y >= 4 && y <= 15
          -- Blade 2 at x=7, 1px wide, height 14 (y=2..15), medium green
          blade2 = x == 7 && y >= 2 && y <= 15
          -- Blade 3 at x=12, 2px wide, height 8 (y=8..15), lighter green
          blade3 = (x == 12 || x == 13) && y >= 8 && y <= 15
          n = pixHash x y 4900 `mod` 20
      in if blade1 then (35 + fromIntegral n, 120 + fromIntegral n, 20, 255)
         else if blade2 then (50 + fromIntegral n, 150 + fromIntegral n, 30, 255)
         else if blade3 then (60 + fromIntegral n, 170 + fromIntegral n, 40, 255)
         else (0, 0, 0, 0)

    -- Dandelion: green stem with bright yellow flower head on transparent bg
    dandelionPattern x y =
      let -- Green stem: x=7-8, y=6-15
          isStem = (x == 7 || x == 8) && y >= 6 && y <= 15
          -- Yellow flower head: circle radius 3 centered at (7.5, 3)
          dx = fromIntegral x - (7.5 :: Double)
          dy = fromIntegral y - (3.0 :: Double)
          dist2 = dx * dx + dy * dy
          isFlower = dist2 <= 3.0 * 3.0
          n = pixHash x y 4950 `mod` 15
      in if isFlower then (255, 220 + fromIntegral (n `div` 3), 50, 255)
         else if isStem then (50, 130 + fromIntegral n, 30, 255)
         else (0, 0, 0, 0)

    -- Rose: green stem with red petals at top and small leaves on transparent bg
    rosePattern x y =
      let -- Green stem: x=7-8, y=8-15
          isStem = (x == 7 || x == 8) && y >= 8 && y <= 15
          -- Red petals: circle radius 3 centered at (7.5, 4)
          dx = fromIntegral x - (7.5 :: Double)
          dy = fromIntegral y - (4.0 :: Double)
          dist2 = dx * dx + dy * dy
          isPetal = dist2 <= 3.0 * 3.0
          -- Small green leaves at y=9-10, flanking the stem
          isLeaf = (y == 9 || y == 10) && ((x >= 5 && x <= 6) || (x >= 9 && x <= 10))
          n = pixHash x y 5050 `mod` 15
      in if isPetal then (220 + fromIntegral (n `div` 3), 30, 30, 255)
         else if isLeaf then (40, 120 + fromIntegral n, 25, 255)
         else if isStem then (50, 130 + fromIntegral n, 30, 255)
         else (0, 0, 0, 0)

    -- Brown mushroom: white-tan stem with brown semicircle cap on transparent bg
    brownMushroomPattern x y =
      let -- Stem: x=7-8, y=10-15, white-tan
          isStem = (x == 7 || x == 8) && y >= 10 && y <= 15
          -- Cap: semicircle at y=4-9, radius 5, centered at (7.5, 9)
          dx = fromIntegral x - (7.5 :: Double)
          dy = fromIntegral y - (9.0 :: Double)
          dist2 = dx * dx + dy * dy
          isCap = dist2 <= 5.0 * 5.0 && y >= 4 && y <= 9
          n = pixHash x y 5000 `mod` 20
      in if isCap then (140 + fromIntegral n, 100 + fromIntegral (n `div` 2), 60, 255)
         else if isStem then (200, 190, 170, 255)
         else (0, 0, 0, 0)

    -- Red mushroom: same stem, red cap with white spots on transparent bg
    redMushroomPattern x y =
      let -- Stem: x=7-8, y=10-15, white-tan
          isStem = (x == 7 || x == 8) && y >= 10 && y <= 15
          -- Cap: semicircle at y=4-9, radius 5, centered at (7.5, 9)
          dx = fromIntegral x - (7.5 :: Double)
          dy = fromIntegral y - (9.0 :: Double)
          dist2 = dx * dx + dy * dy
          isCap = dist2 <= 5.0 * 5.0 && y >= 4 && y <= 9
          -- White spots on cap surface
          isSpot = isCap && ((x == 6 && y == 6) || (x == 9 && y == 5) || (x == 7 && y == 7) || (x == 10 && y == 7))
      in if isSpot then (240, 240, 240, 255)
         else if isCap then (200, 40, 30, 255)
         else if isStem then (200, 190, 170, 255)
         else (0, 0, 0, 0)

    -- Bookshelf side: planks top/bottom with book spines in the middle
    bookshelfSide x y =
      let isPlank = y <= 2 || y >= 13
          isSpine = y >= 4 && y <= 11 && x `mod` 4 /= 0
          n = pixHash x y 5100 `mod` 30
          spineColor = case (x `div` 4) `mod` 4 of
            0 -> (120 + fromIntegral n, 50, 30, 255)   -- red-brown
            1 -> (40, 80 + fromIntegral n, 40, 255)    -- green
            2 -> (30, 50, 120 + fromIntegral n, 255)   -- blue
            _ -> (100 + fromIntegral n, 80, 40, 255)   -- tan
      in if isPlank then planksPattern x y
         else if isSpine then spineColor
         else (60, 40, 20, 255)  -- shelf shadow

    -- Anvil: dark iron body with lighter top
    anvilPattern x y =
      let n = pixHash x y 5200 `mod` 20
          isTop = y <= 4 && x >= 2 && x <= 13
          isBase = y >= 12 && x >= 3 && x <= 12
          isBody = y >= 5 && y <= 11 && x >= 5 && x <= 10
          v = if isTop then fromIntegral (140 + n)
              else if isBody then fromIntegral (100 + n)
              else if isBase then fromIntegral (120 + n)
              else 0
      in if isTop || isBody || isBase then (v, v, fromIntegral (v + 5), 255)
         else (0, 0, 0, 0)

    -- Brewing stand: thin stand with bottles
    brewingStandPattern x y =
      let isPole = x >= 7 && x <= 8 && y >= 2 && y <= 12
          isBase = y >= 13 && ((x >= 2 && x <= 5) || (x >= 10 && x <= 13) || (x >= 7 && x <= 8))
          isBottle = (x >= 3 && x <= 5 && y >= 7 && y <= 11)
                  || (x >= 10 && x <= 12 && y >= 7 && y <= 11)
          n = pixHash x y 5300 `mod` 15
      in if isPole then (120 + fromIntegral n, 120 + fromIntegral n, 130 + fromIntegral n, 255)
         else if isBottle then (180, 180, 200, 200)
         else if isBase then (80, 80, 90, 255)
         else (0, 0, 0, 0)

    -- Ice: pale blue-white base with crack lines and frosted patches
    icePattern x y =
      let -- Crack line 1: jagged horizontal path near y=5
          crack1 = let cy = 5 + (pixHash x 0 5410 `mod` 3) - 1
                   in y == cy && x >= 1 && x <= 14
          -- Crack line 2: jagged diagonal from top-right to bottom-left
          crack2 = let cy = x + 3 + (pixHash x 1 5411 `mod` 3) - 1
                   in y == cy && cy >= 0 && cy <= 15
          -- Crack line 3: jagged vertical path near x=10
          crack3 = let cx = 10 + (pixHash 0 y 5412 `mod` 3) - 1
                   in x == cx && y >= 2 && y <= 13
          isCrack = crack1 || crack2 || crack3
          -- Frosted patches: ~20% of pixels are lighter
          frosted = pixHash x y 5420 `mod` 100 < 20
          (r, g, b) = if isCrack then (160, 190, 220)
                      else if frosted then (230, 240, 250)
                      else (200, 220, 240)
      in (r, g, b, 220)

    -- Packed ice: solid opaque blue-white with dense cracks
    packedIcePattern x y =
      let n = pixHash x y 5500 `mod` 100
          crack = pixHash (x `div` 3) (y `div` 3) 5501 `mod` 10 < 3
          (r, g, b) = if crack then (180, 200, 230)
                      else if n < 30 then (130, 160, 200)
                      else (150, 180, 220)
      in (r, g, b, 255)

    -- Redstone lamp: glowing orange-yellow with grid pattern
    redstoneLampPattern x y =
      let n = pixHash x y 5600 `mod` 100
          border = x == 0 || x == 15 || y == 0 || y == 15
          grid = x `mod` 4 == 0 || y `mod` 4 == 0
          (r, g, b) = if border then (100, 50, 20)
                      else if grid then (160, 80, 30)
                      else (220 + fromIntegral (n `mod` 35), 160 + fromIntegral (n `mod` 40), 60)
      in (r, g, b, 255)

    -- Hopper: dark iron funnel shape
    hopperPattern x y =
      let n = pixHash x y 5700 `mod` 20
          border = x == 0 || x == 15
          isFunnel = x >= (y `div` 2) && x <= (15 - y `div` 2)
          v = if border then fromIntegral (60 + n)
              else if isFunnel then fromIntegral (100 + n)
              else 0
      in if border || isFunnel then (v, v, fromIntegral (v + 3), 255)
         else (0, 0, 0, 0)

    -- Hopper top: dark iron with center opening
    hopperTopPattern x y =
      let n = pixHash x y 5710 `mod` 20
          isOpening = x >= 4 && x <= 11 && y >= 4 && y <= 11
          border = x == 0 || x == 15 || y == 0 || y == 15
          v = if isOpening then fromIntegral (30 + n)
              else if border then fromIntegral (80 + n)
              else fromIntegral (110 + n)
      in (v, v, fromIntegral (v + 3), 255)

-- | Create a texture from raw RGBA pixel data
createTextureFromPixels
  :: Vk.PhysicalDevice -> Vk.Device -> Vk.CommandPool -> Vk.Queue
  -> Word32 -> Word32 -> VS.Vector Word8
  -> IO TextureImage
createTextureFromPixels physDevice device cmdPool queue width height pixels = do
  let imageSize = fromIntegral $ VS.length pixels

  -- Create staging buffer
  staging <- createBuffer physDevice device imageSize
    Vk.BUFFER_USAGE_TRANSFER_SRC_BIT
    (Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. Vk.MEMORY_PROPERTY_HOST_COHERENT_BIT)

  -- Upload pixel data
  ptr <- Vk.mapMemory device (baMemory staging) 0 imageSize Vk.zero
  VS.unsafeWith pixels $ \srcPtr ->
    copyBytes (castPtr ptr) srcPtr (VS.length pixels)
  Vk.unmapMemory device (baMemory staging)

  -- Create image
  let imageInfo = Vk.ImageCreateInfo
        { Vk.next          = ()
        , Vk.flags         = Vk.zero
        , Vk.imageType     = Vk.IMAGE_TYPE_2D
        , Vk.format        = Vk.FORMAT_R8G8B8A8_SRGB
        , Vk.extent        = Vk.Extent3D width height 1
        , Vk.mipLevels     = 1
        , Vk.arrayLayers   = 1
        , Vk.samples       = Vk.SAMPLE_COUNT_1_BIT
        , Vk.tiling        = Vk.IMAGE_TILING_OPTIMAL
        , Vk.usage         = Vk.IMAGE_USAGE_TRANSFER_DST_BIT .|. Vk.IMAGE_USAGE_SAMPLED_BIT
        , Vk.sharingMode   = Vk.SHARING_MODE_EXCLUSIVE
        , Vk.queueFamilyIndices = V.empty
        , Vk.initialLayout = Vk.IMAGE_LAYOUT_UNDEFINED
        }
  image <- Vk.createImage device imageInfo Nothing

  -- Allocate and bind memory
  memReqs <- Vk.getImageMemoryRequirements device image
  memProps <- Vk.getPhysicalDeviceMemoryProperties physDevice
  let Vk.MemoryRequirements{memoryTypeBits = reqMemTypeBits, size = reqSize} = memReqs
  let memTypeIdx = findMemType reqMemTypeBits Vk.MEMORY_PROPERTY_DEVICE_LOCAL_BIT memProps
  let allocInfo = Vk.MemoryAllocateInfo
        { Vk.next            = ()
        , Vk.allocationSize  = reqSize
        , Vk.memoryTypeIndex = memTypeIdx
        }
  memory <- Vk.allocateMemory device allocInfo Nothing
  Vk.bindImageMemory device image memory 0

  -- Transition image layout and copy buffer to image
  transitionImageLayout device cmdPool queue image
    Vk.IMAGE_LAYOUT_UNDEFINED Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
  copyBufferToImage device cmdPool queue (baBuffer staging) image width height
  transitionImageLayout device cmdPool queue image
    Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL

  destroyBuffer device staging

  -- Create image view
  let viewInfo = Vk.ImageViewCreateInfo
        { Vk.next             = ()
        , Vk.flags            = Vk.zero
        , Vk.image            = image
        , Vk.viewType         = Vk.IMAGE_VIEW_TYPE_2D
        , Vk.format           = Vk.FORMAT_R8G8B8A8_SRGB
        , Vk.components       = Vk.ComponentMapping
            Vk.COMPONENT_SWIZZLE_IDENTITY Vk.COMPONENT_SWIZZLE_IDENTITY
            Vk.COMPONENT_SWIZZLE_IDENTITY Vk.COMPONENT_SWIZZLE_IDENTITY
        , Vk.subresourceRange = Vk.ImageSubresourceRange
            Vk.IMAGE_ASPECT_COLOR_BIT 0 1 0 1
        }
  imageView <- Vk.createImageView device viewInfo Nothing

  -- Create sampler with nearest-neighbor filtering (Minecraft pixel art style)
  let samplerInfo = Vk.SamplerCreateInfo
        { Vk.next                   = ()
        , Vk.flags                  = Vk.zero
        , Vk.magFilter              = Vk.FILTER_NEAREST
        , Vk.minFilter              = Vk.FILTER_NEAREST
        , Vk.mipmapMode             = Vk.SAMPLER_MIPMAP_MODE_NEAREST
        , Vk.addressModeU           = Vk.SAMPLER_ADDRESS_MODE_REPEAT
        , Vk.addressModeV           = Vk.SAMPLER_ADDRESS_MODE_REPEAT
        , Vk.addressModeW           = Vk.SAMPLER_ADDRESS_MODE_REPEAT
        , Vk.mipLodBias             = 0
        , Vk.anisotropyEnable       = False
        , Vk.maxAnisotropy          = 1
        , Vk.compareEnable          = False
        , Vk.compareOp              = Vk.COMPARE_OP_ALWAYS
        , Vk.minLod                 = 0
        , Vk.maxLod                 = 0
        , Vk.borderColor            = Vk.BORDER_COLOR_INT_OPAQUE_BLACK
        , Vk.unnormalizedCoordinates = False
        }
  sampler <- Vk.createSampler device samplerInfo Nothing

  pure $ TextureImage image memory imageView sampler

-- | Create a full texture atlas from a PNG file
createTextureAtlas
  :: Vk.PhysicalDevice -> Vk.Device -> Vk.CommandPool -> Vk.Queue
  -> FilePath
  -> IO TextureImage
createTextureAtlas _physDevice _device _cmdPool _queue _path =
  -- TODO: implement PNG loading with JuicyPixels
  error "createTextureAtlas: not yet implemented, use createPlaceholderAtlas"

-- | Destroy texture image resources
destroyTextureImage :: Vk.Device -> TextureImage -> IO ()
destroyTextureImage device ti = do
  Vk.destroySampler device (tiSampler ti) Nothing
  Vk.destroyImageView device (tiImageView ti) Nothing
  Vk.destroyImage device (tiImage ti) Nothing
  Vk.freeMemory device (tiMemory ti) Nothing

-- | Transition image layout using a one-shot command buffer
transitionImageLayout :: Vk.Device -> Vk.CommandPool -> Vk.Queue -> Vk.Image -> Vk.ImageLayout -> Vk.ImageLayout -> IO ()
transitionImageLayout device cmdPool queue image oldLayout newLayout = do
  let (srcAccess, dstAccess, srcStage, dstStage) = layoutTransitionMasks oldLayout newLayout
  let barrier = Vk.ImageMemoryBarrier
        { Vk.next                = ()
        , Vk.srcAccessMask       = srcAccess
        , Vk.dstAccessMask       = dstAccess
        , Vk.oldLayout           = oldLayout
        , Vk.newLayout           = newLayout
        , Vk.srcQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED
        , Vk.dstQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED
        , Vk.image               = image
        , Vk.subresourceRange    = Vk.ImageSubresourceRange
            Vk.IMAGE_ASPECT_COLOR_BIT 0 1 0 1
        }
  withOneShot device cmdPool queue $ \cmdBuf ->
    Vk.cmdPipelineBarrier cmdBuf srcStage dstStage Vk.zero
      V.empty V.empty (V.singleton (Vk.SomeStruct barrier))

-- | Copy buffer to image
copyBufferToImage :: Vk.Device -> Vk.CommandPool -> Vk.Queue -> Vk.Buffer -> Vk.Image -> Word32 -> Word32 -> IO ()
copyBufferToImage device cmdPool queue buffer image width height = do
  let region = Vk.BufferImageCopy
        { Vk.bufferOffset      = 0
        , Vk.bufferRowLength   = 0
        , Vk.bufferImageHeight = 0
        , Vk.imageSubresource  = Vk.ImageSubresourceLayers Vk.IMAGE_ASPECT_COLOR_BIT 0 0 1
        , Vk.imageOffset       = Vk.Offset3D 0 0 0
        , Vk.imageExtent       = Vk.Extent3D width height 1
        }
  withOneShot device cmdPool queue $ \cmdBuf ->
    Vk.cmdCopyBufferToImage cmdBuf buffer image Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL (V.singleton region)

-- | Run a one-shot command buffer
withOneShot :: Vk.Device -> Vk.CommandPool -> Vk.Queue -> (Vk.CommandBuffer -> IO ()) -> IO ()
withOneShot device cmdPool queue action = do
  let allocInfo = Vk.CommandBufferAllocateInfo cmdPool Vk.COMMAND_BUFFER_LEVEL_PRIMARY 1
  cmdBuffers <- Vk.allocateCommandBuffers device allocInfo
  let cmdBuf = V.head cmdBuffers
  let beginInfo = Vk.CommandBufferBeginInfo
        { Vk.next = (), Vk.flags = Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT, Vk.inheritanceInfo = Nothing }
  Vk.beginCommandBuffer cmdBuf beginInfo
  action cmdBuf
  Vk.endCommandBuffer cmdBuf
  let submitInfo = Vk.SubmitInfo
        { Vk.next = (), Vk.waitSemaphores = V.empty, Vk.waitDstStageMask = V.empty
        , Vk.commandBuffers = V.singleton (Vk.commandBufferHandle cmdBuf), Vk.signalSemaphores = V.empty }
  Vk.queueSubmit queue (V.singleton (Vk.SomeStruct submitInfo)) Vk.zero
  Vk.queueWaitIdle queue
  Vk.freeCommandBuffers device cmdPool cmdBuffers

-- | Layout transition access masks and pipeline stages
layoutTransitionMasks
  :: Vk.ImageLayout -> Vk.ImageLayout
  -> (Vk.AccessFlags, Vk.AccessFlags, Vk.PipelineStageFlags, Vk.PipelineStageFlags)
layoutTransitionMasks Vk.IMAGE_LAYOUT_UNDEFINED Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL =
  (Vk.zero, Vk.ACCESS_TRANSFER_WRITE_BIT, Vk.PIPELINE_STAGE_TOP_OF_PIPE_BIT, Vk.PIPELINE_STAGE_TRANSFER_BIT)
layoutTransitionMasks Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL =
  (Vk.ACCESS_TRANSFER_WRITE_BIT, Vk.ACCESS_SHADER_READ_BIT, Vk.PIPELINE_STAGE_TRANSFER_BIT, Vk.PIPELINE_STAGE_FRAGMENT_SHADER_BIT)
layoutTransitionMasks _ _ = error "Unsupported layout transition"

-- | Find memory type index
findMemType :: Word32 -> Vk.MemoryPropertyFlags -> Vk.PhysicalDeviceMemoryProperties -> Word32
findMemType typeFilter props memProps =
  let memTypes = Vk.memoryTypes memProps
      count = Vk.memoryTypeCount memProps
      go i
        | i >= fromIntegral count = error "Failed to find suitable memory type for texture"
        | typeFilter `testBit'` i
          && (Vk.propertyFlags (memTypes V.! fromIntegral i) .&. props) == props
          = i
        | otherwise = go (i + 1)
  in go 0
  where
    testBit' bits i = bits .&. (1 `shiftL` fromIntegral i) /= 0

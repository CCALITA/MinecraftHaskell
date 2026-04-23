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

    -- Grass top: green with darker blade variations
    grassTop x y =
      let n = pixHash x y 42 `mod` 100
          base = if n < 30 then (30, 140, 20) else if n < 60 then (50, 170, 30) else (40, 155, 25)
          (br, bg, bb) = base
      in (br, bg, bb, 255)

    -- Stone: gray with crack-like darker lines
    stonePattern x y =
      let n = pixHash x y 100 `mod` 100
          crack = pixHash (x `div` 3) (y `div` 4) 101 `mod` 10 < 2
          base = if crack then 95 else if n < 20 then 115 else if n < 50 then 125 else 130
      in (base, base, base, 255)

    -- Dirt: brown with speckled variation
    dirtPattern x y =
      let n = pixHash x y 200 `mod` 100
          r = if n < 25 then 120 else if n < 50 then 135 else 140
          g = r * 60 `div` 140
          b = r * 35 `div` 140
      in (fromIntegral r, fromIntegral g, fromIntegral b, 255)

    -- Grass side: dirt bottom with green strip on top
    grassSide x y
      | y <= 2    = grassTop x y
      | y == 3    = let n = pixHash x y 300 `mod` 3
                    in if n == 0 then grassTop x y else dirtPattern x y
      | otherwise = dirtPattern x y

    -- Wood planks: tan with horizontal grain lines
    planksPattern x y =
      let grain = pixHash x (y `div` 4) 400 `mod` 100
          base = if y `mod` 4 == 0 then 160 else if grain < 20 then 190 else 200
          r = base; g = base * 170 `div` 210; b = base * 110 `div` 210
      in (fromIntegral r, fromIntegral g, fromIntegral b, 255)

    -- Gravel: mix of gray pebble shapes
    gravelPattern x y =
      let n = pixHash x y 500 `mod` 100
          v = if n < 20 then 100 else if n < 50 then 130 else if n < 80 then 145 else 160
      in (v, v, v, 255)

    -- Log bark: brown with vertical lines
    logBark x y =
      let stripe = pixHash (x `div` 2) y 600 `mod` 100
          dark = x `mod` 3 == 0 || stripe < 15
          (r, g, b) = if dark then (70, 50, 25) else (100, 75, 40)
      in (r, g, b, 255)

    -- Leaves: transparent-ish green with holes
    leavesPattern x y =
      let n = pixHash x y 700 `mod` 100
          hole = n < 15
      in if hole then (20, 60, 15, 180) else (40 + fromIntegral (n `mod` 30), 120 + fromIntegral (n `mod` 40), 30, 255)

    -- Cobblestone: irregular gray blocks
    cobblePattern x y =
      let blockN = pixHash (x `div` 4) (y `div` 3) 800
          border = x `mod` 4 == 0 || y `mod` 3 == 0
          v = if border then 80 else fromIntegral (100 + blockN `mod` 40)
      in (v, v, v, 255)

    -- Bedrock: very dark with slight variation
    bedrockPattern x y =
      let n = pixHash x y 900 `mod` 30
          v = fromIntegral (30 + n)
      in (v, v, v, 255)

    -- Ore: stone base with colored spots
    orePattern x y (or', og, ob) =
      let isOreSpot = let n = pixHash (x `div` 3) (y `div` 3) 1000 `mod` 10
                      in n < 3 && (x + y) `mod` 3 /= 0
      in if isOreSpot then (or', og, ob, 255) else stonePattern x y

    -- Water: blue with subtle wave pattern
    waterPattern x y =
      let wave = pixHash x (y + x `div` 3) 1100 `mod` 30
          b = fromIntegral (160 + wave)
          g = fromIntegral (80 + wave `div` 2)
      in (30, g, b, 200)

    -- Lava: orange-red with bright spots
    lavaPattern x y =
      let n = pixHash x y 1200 `mod` 100
          bright = n < 20
          (r, g, b) = if bright then (255, 200, 50) else (220, 80 + fromIntegral (n `mod` 30), 10)
      in (r, g, b, 255)

    -- Glass: mostly transparent with faint border
    glassPattern x y =
      let border = x == 0 || y == 0 || x == 15 || y == 15
      in if border then (200, 220, 240, 120) else (220, 235, 250, 40)

    -- Log cross-section (top/bottom of oak log)
    logTop x y =
      let dist = abs (x - 7) + abs (y - 7)  -- taxicab distance from center
          ring = dist `div` 2
      in if ring <= 1 then (80, 60, 30, 255)    -- dark center
         else if ring <= 3 then (160, 130, 70, 255) -- light wood
         else (100, 75, 40, 255)                 -- bark edge

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

    -- TNT side: red with white band
    tntSide x y =
      let band = y >= 5 && y <= 10
      in if band then (220, 220, 220, 255) else (200, 50, 40, 255)

    tntTop _ _ = (180, 180, 180, 255)
    tntBottom _ _ = (180, 180, 180, 255)

    -- Chest: brown wood box
    chestPattern x y =
      let border = x == 0 || y == 0 || x == 15 || y == 15
          latch = x >= 6 && x <= 9 && y >= 6 && y <= 8
      in if latch then (200, 180, 50, 255)  -- gold latch
         else if border then (100, 65, 30, 255) -- dark border
         else (140, 95, 45, 255)  -- wood

    -- Furnace side: stone with dark opening
    furnaceSide x y =
      let opening = x >= 4 && x <= 11 && y >= 5 && y <= 12
      in if opening then (40, 40, 40, 255) else stonePattern x y

    furnaceTop = stonePattern

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

    -- Crafting table top: wooden grid pattern
    craftingTop x y =
      let grid = x `mod` 8 == 0 || y `mod` 8 == 0
      in if grid then (80, 55, 25, 255) else planksPattern x y

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

    -- Torch: mostly transparent with orange flame center
    torchPattern x y =
      let isStick = x >= 7 && x <= 8 && y >= 5 && y <= 15
          isFlame = x >= 6 && x <= 9 && y >= 2 && y <= 6
      in if isFlame then (255, 200, 50, 255)
         else if isStick then (120, 90, 40, 255)
         else (0, 0, 0, 0)  -- transparent

    -- Obsidian: very dark purple/black with faint purple streaks
    obsidianPattern x y =
      let n = pixHash x y 2000 `mod` 100
          streak = pixHash (x `div` 3) (y `div` 2) 2001 `mod` 10 < 2
          (r, g, b) = if streak then (30, 10, 40) else if n < 30 then (15, 10, 20) else (20, 15, 25)
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

    -- Oak sapling: small green cross on brown stem
    oakSaplingPattern x y =
      let isStem = x >= 7 && x <= 8 && y >= 10 && y <= 15
          isLeaf = (x >= 5 && x <= 10 && y >= 4 && y <= 10)
                   && not (x == 5 && y == 4) && not (x == 10 && y == 4)
                   && not (x == 5 && y == 10) && not (x == 10 && y == 10)
          n = pixHash x y 2300 `mod` 30
      in if isLeaf then (30 + fromIntegral n, 120 + fromIntegral n, 20, 255)
         else if isStem then (100, 70, 30, 255)
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
    -- Dispenser front: stone-like body with dark opening hole in the center
    dispenserFrontPattern x y =
      let isOpening = x >= 5 && x <= 10 && y >= 5 && y <= 10
          border = x == 0 || x == 15 || y == 0 || y == 15
          n = pixHash x y 2700 `mod` 20
          baseV = fromIntegral (120 + n) :: Word8
      in if isOpening then (30, 30, 35, 255)
         else if border then (90, 90, 95, 255)
         else (baseV, baseV, fromIntegral (baseV + 3), 255)

    -- Enchanting table top: dark obsidian base with diamond-studded pattern and runes
    enchantingTableTop x y =
      let n = pixHash x y 2700 `mod` 100
          -- Diamond studs at corners and center
          isDiamond = (x == 2 && y == 2) || (x == 13 && y == 2)
                   || (x == 2 && y == 13) || (x == 13 && y == 13)
                   || (x >= 7 && x <= 8 && y >= 7 && y <= 8)
          -- Rune symbols (glowing purple lines)
          isRune = (x == 5 && y >= 4 && y <= 11)
                || (x == 10 && y >= 4 && y <= 11)
                || (y == 4 && x >= 5 && x <= 10)
                || (y == 11 && x >= 5 && x <= 10)
          -- Border
          border = x == 0 || x == 15 || y == 0 || y == 15
      in if isDiamond then (100, 220, 255, 255)  -- bright cyan diamond
         else if isRune then (120, 50, 180, 255)  -- glowing purple rune
         else if border then (15, 8, 20, 255)     -- dark border
         else if n < 20 then (25, 12, 35, 255)    -- dark obsidian variant
         else (20, 10, 30, 255)                    -- obsidian base

    -- Enchanting table side: dark obsidian with subtle purple glow
    enchantingTableSide x y =
      let n = pixHash x y 2800 `mod` 100
          -- Purple accents along the middle band
          accentBand = y >= 6 && y <= 9
          border = x == 0 || x == 15 || y == 0 || y == 15
          streak = pixHash (x `div` 3) (y `div` 2) 2801 `mod` 10 < 2
      in if border then (15, 8, 20, 255)
         else if accentBand && streak then (80, 30, 120, 255)  -- purple accent
         else if streak then (30, 15, 40, 255)                 -- subtle purple streak
         else if n < 30 then (20, 10, 28, 255)
         else (25, 12, 32, 255)

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

    -- Birch bark: white-gray with dark horizontal bands
    birchBark x y =
      let band = y `mod` 5 == 0 || y `mod` 5 == 1
          n = pixHash x y 4000 `mod` 20
          (r, g, b) = if band then (50 + fromIntegral n, 45 + fromIntegral n, 40 + fromIntegral n)
                      else (200 + fromIntegral (n `div` 2), 195 + fromIntegral (n `div` 2), 185 + fromIntegral (n `div` 2))
      in (r, g, b, 255)

    -- Birch log cross-section
    birchLogTop x y =
      let dist = abs (x - 7) + abs (y - 7)
          ring = dist `div` 2
      in if ring <= 1 then (180, 160, 120, 255)
         else if ring <= 3 then (210, 200, 160, 255)
         else (200, 195, 185, 255)

    -- Birch leaves: lighter green with yellowish tint
    birchLeavesPattern x y =
      let n = pixHash x y 4100 `mod` 100
          hole = n < 15
      in if hole then (30, 70, 20, 180)
         else (60 + fromIntegral (n `mod` 30), 140 + fromIntegral (n `mod` 30), 40 + fromIntegral (n `mod` 20), 255)

    -- Birch planks: lighter than oak, pale yellowish
    birchPlanksPattern x y =
      let grain = pixHash x (y `div` 4) 4200 `mod` 100
          base = if y `mod` 4 == 0 then 190 else if grain < 20 then 210 else 220
          r = base; g = base * 200 `div` 230; b = base * 160 `div` 230
      in (fromIntegral r, fromIntegral g, fromIntegral b, 255)

    -- Spruce bark: dark brown, almost black
    spruceBark x y =
      let stripe = pixHash (x `div` 2) y 4300 `mod` 100
          dark = x `mod` 3 == 0 || stripe < 15
          (r, g, b) = if dark then (35, 25, 15) else (55, 40, 25)
      in (r, g, b, 255)

    -- Spruce log cross-section
    spruceLogTop x y =
      let dist = abs (x - 7) + abs (y - 7)
          ring = dist `div` 2
      in if ring <= 1 then (50, 35, 20, 255)
         else if ring <= 3 then (120, 90, 50, 255)
         else (55, 40, 25, 255)

    -- Spruce leaves: dark blue-green
    spruceLeavesPattern x y =
      let n = pixHash x y 4400 `mod` 100
          hole = n < 12
      in if hole then (10, 30, 15, 180)
         else (20 + fromIntegral (n `mod` 20), 60 + fromIntegral (n `mod` 30), 25 + fromIntegral (n `mod` 15), 255)

    -- Spruce planks: darker than oak
    sprucePlanksPattern x y =
      let grain = pixHash x (y `div` 4) 4500 `mod` 100
          base = if y `mod` 4 == 0 then 110 else if grain < 20 then 130 else 140
          r = base; g = base * 85 `div` 140; b = base * 55 `div` 140
      in (fromIntegral r, fromIntegral g, fromIntegral b, 255)

    -- Jungle bark: pale with green-brown tint and vertical lines
    jungleBark x y =
      let stripe = pixHash (x `div` 2) y 4600 `mod` 100
          dark = x `mod` 4 == 0 || stripe < 12
          (r, g, b) = if dark then (80, 65, 35) else (120, 100, 55)
      in (r, g, b, 255)

    -- Jungle log cross-section
    jungleLogTop x y =
      let dist = abs (x - 7) + abs (y - 7)
          ring = dist `div` 2
      in if ring <= 1 then (90, 70, 35, 255)
         else if ring <= 3 then (150, 130, 70, 255)
         else (120, 100, 55, 255)

    -- Jungle leaves: vibrant green
    jungleLeavesPattern x y =
      let n = pixHash x y 4700 `mod` 100
          hole = n < 10
      in if hole then (15, 50, 10, 180)
         else (30 + fromIntegral (n `mod` 25), 100 + fromIntegral (n `mod` 40), 20 + fromIntegral (n `mod` 15), 255)

    -- Jungle planks: warm reddish-brown
    junglePlanksPattern x y =
      let grain = pixHash x (y `div` 4) 4800 `mod` 100
          base = if y `mod` 4 == 0 then 150 else if grain < 20 then 170 else 180
          r = base; g = base * 110 `div` 180; b = base * 75 `div` 180
      in (fromIntegral r, fromIntegral g, fromIntegral b, 255)

    -- Tall grass: green blades on transparent background
    tallGrassPattern x y =
      let isBlade = (x `mod` 3 == 1 || x `mod` 5 == 2) && y >= 3
          n = pixHash x y 4900 `mod` 50
      in if isBlade then (40 + fromIntegral n, 130 + fromIntegral (n `div` 2), 25, 255)
         else (0, 0, 0, 0)

    -- Dandelion: yellow flower on green stem
    dandelionPattern x y =
      let isFlower = x >= 5 && x <= 10 && y >= 2 && y <= 6
          isStem = x >= 7 && x <= 8 && y >= 7 && y <= 14
      in if isFlower then (255, 220, 50, 255)
         else if isStem then (50, 120, 30, 255)
         else (0, 0, 0, 0)

    -- Rose: red flower on green stem
    rosePattern x y =
      let isFlower = x >= 5 && x <= 10 && y >= 2 && y <= 7
                     && not (x == 5 && y == 2) && not (x == 10 && y == 2)
          isStem = x >= 7 && x <= 8 && y >= 8 && y <= 14
      in if isFlower then (200, 30, 30, 255)
         else if isStem then (50, 120, 30, 255)
         else (0, 0, 0, 0)

    -- Brown mushroom: brown cap on thin stem
    brownMushroomPattern x y =
      let isCap = x >= 4 && x <= 11 && y >= 3 && y <= 8
                  && not (x == 4 && y == 3) && not (x == 11 && y == 3)
          isStem = x >= 7 && x <= 8 && y >= 9 && y <= 14
          n = pixHash x y 5000 `mod` 20
      in if isCap then (140 + fromIntegral n, 100 + fromIntegral (n `div` 2), 60, 255)
         else if isStem then (200, 190, 170, 255)
         else (0, 0, 0, 0)

    -- Red mushroom: red cap with white spots on thin stem
    redMushroomPattern x y =
      let isCap = x >= 4 && x <= 11 && y >= 3 && y <= 8
                  && not (x == 4 && y == 3) && not (x == 11 && y == 3)
          isSpot = isCap && ((x == 6 && y == 5) || (x == 9 && y == 4) || (x == 7 && y == 7))
          isStem = x >= 7 && x <= 8 && y >= 9 && y <= 14
      in if isSpot then (240, 240, 240, 255)
         else if isCap then (200, 30, 30, 255)
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

    -- Ice: light blue semi-transparent with white cracks
    icePattern x y =
      let n = pixHash x y 5400 `mod` 100
          crack = pixHash (x `div` 4) (y `div` 3) 5401 `mod` 10 < 2
          (r, g, b, a) = if crack then (220, 230, 255, 200)
                         else if n < 30 then (140, 180, 220, 180)
                         else (160, 200, 240, 190)
      in (r, g, b, a)

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

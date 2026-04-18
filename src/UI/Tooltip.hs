module UI.Tooltip
  ( TooltipInfo(..)
  , buildTooltip
  , renderTooltipVertices
  ) where

import Game.Item
import Game.Enchanting (Enchantment(..), enchantmentName)
import Engine.BitmapFont (renderText, charSpacing, charHeight)
import World.Block (BlockType(..))

-- | Tooltip display data, built from an Item and its enchantments.
data TooltipInfo = TooltipInfo
  { ttName         :: !String
  , ttLoreLines    :: ![String]       -- colored description lines
  , ttEnchantments :: ![String]       -- "Sharpness III", "Unbreaking II"
  , ttDurability   :: !(Maybe (Int, Int))  -- (current, max) if applicable
  } deriving stock (Show, Eq)

-- ---------------------------------------------------------------------------
-- Build tooltip from item + enchantments
-- ---------------------------------------------------------------------------

-- | Construct a TooltipInfo for an item with optional enchantments.
buildTooltip :: Item -> [Enchantment] -> TooltipInfo
buildTooltip item enchants = TooltipInfo
  { ttName         = itemName item
  , ttLoreLines    = itemLore item
  , ttEnchantments = fmap formatEnchantment enchants
  , ttDurability   = itemDurability item
  }

-- | Human-readable name for any item.
itemName :: Item -> String
itemName (BlockItem bt)         = blockTypeName bt
itemName (ToolItem tt tm _)     = toolMaterialName tm ++ " " ++ toolTypeName tt
itemName StickItem              = "Stick"
itemName (FoodItem ft)          = foodName ft
itemName (MaterialItem mt)      = materialName mt
itemName (ArmorItem slot mat _) = armorMaterialName mat ++ " " ++ armorSlotName slot
itemName (ShearsItem _)         = "Shears"
itemName (FlintAndSteelItem _)  = "Flint and Steel"
itemName CompassItem            = "Compass"
itemName ClockItem              = "Clock"
itemName (FishingRodItem _)     = "Fishing Rod"
itemName GlassBottleItem        = "Glass Bottle"
itemName (PotionItem pt)        = potionName pt
itemName BoatItem               = "Boat"
itemName MinecartItem           = "Minecart"

-- | Extra description lines (lore) for certain item categories.
itemLore :: Item -> [String]
itemLore (FoodItem ft)          = ["Restores " ++ show (foodHungerRestore ft) ++ " hunger"]
itemLore (ArmorItem slot mat _) = ["Defense: " ++ show (armorDefensePoints slot mat)]
itemLore (PotionItem pt)        = [potionLore pt]
itemLore _                      = []

-- | Lore text for a potion type.
potionLore :: PotionType -> String
potionLore WaterBottle   = "No effect"
potionLore AwkwardPotion = "No effect"
potionLore PoisonPotion  = "Inflicts poison"
potionLore HealingPotion = "Restores health"
potionLore SpeedPotion   = "Increases speed"

-- | Durability info (current, max) for items that have it.
itemDurability :: Item -> Maybe (Int, Int)
itemDurability (ToolItem _ mat cur) = Just (cur, tiMaxDurability (toolInfo mat))
itemDurability (ArmorItem _ mat cur) = Just (cur, armorMaxDurability mat)
itemDurability (ShearsItem cur)     = Just (cur, 238)
itemDurability (FlintAndSteelItem cur) = Just (cur, 64)
itemDurability (FishingRodItem cur) = Just (cur, 64)
itemDurability _                    = Nothing

-- | Maximum durability for armor materials.
armorMaxDurability :: ArmorMaterial -> Int
armorMaxDurability LeatherArmor = 80
armorMaxDurability IronArmor    = 240
armorMaxDurability GoldArmor    = 112
armorMaxDurability DiamondArmor = 528

-- ---------------------------------------------------------------------------
-- Rendering
-- ---------------------------------------------------------------------------

-- | Render tooltip vertices at NDC position (x, y).
--   Returns HUD vertex data as [x, y, r, g, b, a, ...] (6 floats per vertex).
--   Layout top-to-bottom: background, name, lore lines, enchantments, durability bar.
renderTooltipVertices :: TooltipInfo -> Float -> Float -> [Float]
renderTooltipVertices info x y =
  let scale     = 1.0
      lineH     = charHeight * scale * 7.0   -- line spacing
      padX      = 0.01                       -- horizontal padding
      padY      = 0.008                      -- vertical padding

      -- Gather all text lines
      allLines  = [ttName info]
                  ++ ttLoreLines info
                  ++ ttEnchantments info
                  ++ durabilityLine info
      lineCount = length allLines

      -- Measure longest line for background width
      maxLen    = maximum (fmap length allLines)
      bgW       = fromIntegral maxLen * charSpacing * scale + padX * 2
      bgH       = fromIntegral lineCount * lineH + padY * 2
                  + durabilityBarExtra info

      -- Background rectangle (dark semi-transparent)
      bgColor   = (0.1, 0.1, 0.1, 0.85)
      bgVerts   = quadVerts x y bgW bgH bgColor

      -- Text lines
      textX     = x + padX
      nameY     = y + padY
      nameVerts = renderText textX nameY scale (1.0, 1.0, 1.0, 1.0) (ttName info)

      loreStart = nameY + lineH
      loreVerts = concatMap (\(i, ln) ->
          renderText textX (loreStart + fromIntegral i * lineH)
                     scale (0.6, 0.6, 0.6, 1.0) ln
        ) (zip [0 :: Int ..] (ttLoreLines info))

      enchStart = loreStart + fromIntegral (length (ttLoreLines info)) * lineH
      enchVerts = concatMap (\(i, ln) ->
          renderText textX (enchStart + fromIntegral i * lineH)
                     scale (0.3, 0.5, 1.0, 1.0) ln
        ) (zip [0 :: Int ..] (ttEnchantments info))

      durStart  = enchStart + fromIntegral (length (ttEnchantments info)) * lineH
      durVerts  = renderDurabilityBar info textX durStart bgW padX

  in bgVerts ++ nameVerts ++ loreVerts ++ enchVerts ++ durVerts

-- | Extra height reserved for the durability bar when present.
durabilityBarExtra :: TooltipInfo -> Float
durabilityBarExtra info = case ttDurability info of
  Just _  -> 0.02
  Nothing -> 0.0

-- | Text line for durability, used only for line counting.
durabilityLine :: TooltipInfo -> [String]
durabilityLine info = case ttDurability info of
  Just (cur, mx) -> [show cur ++ "/" ++ show mx]
  Nothing        -> []

-- | Render a green durability bar (filled portion) plus gray background bar.
renderDurabilityBar :: TooltipInfo -> Float -> Float -> Float -> Float -> [Float]
renderDurabilityBar info textX barY bgW padX = case ttDurability info of
  Nothing       -> []
  Just (cur, mx) ->
    let barW   = bgW - padX * 2
        barH   = 0.012
        frac   = fromIntegral cur / fromIntegral (max 1 mx)
        bgBar  = quadVerts textX barY barW barH (0.3, 0.3, 0.3, 1.0)
        fillBar = quadVerts textX barY (barW * frac) barH (0.2, 0.8, 0.2, 1.0)
    in bgBar ++ fillBar

-- | Emit 6 vertices (2 triangles) for a filled rectangle.
quadVerts :: Float -> Float -> Float -> Float -> (Float, Float, Float, Float) -> [Float]
quadVerts qx qy w h (r, g, b, a) =
  [ qx,     qy,     r, g, b, a
  , qx + w, qy,     r, g, b, a
  , qx + w, qy + h, r, g, b, a
  , qx,     qy,     r, g, b, a
  , qx + w, qy + h, r, g, b, a
  , qx,     qy + h, r, g, b, a
  ]

-- ---------------------------------------------------------------------------
-- Name helpers
-- ---------------------------------------------------------------------------

blockTypeName :: BlockType -> String
blockTypeName Air            = "Air"
blockTypeName Stone          = "Stone"
blockTypeName Dirt           = "Dirt"
blockTypeName Grass          = "Grass Block"
blockTypeName Sand           = "Sand"
blockTypeName Gravel         = "Gravel"
blockTypeName OakLog         = "Oak Log"
blockTypeName OakLeaves      = "Oak Leaves"
blockTypeName Water          = "Water"
blockTypeName Lava           = "Lava"
blockTypeName Cobblestone    = "Cobblestone"
blockTypeName OakPlanks      = "Oak Planks"
blockTypeName Glass          = "Glass"
blockTypeName Bedrock        = "Bedrock"
blockTypeName IronOre        = "Iron Ore"
blockTypeName CoalOre        = "Coal Ore"
blockTypeName GoldOre        = "Gold Ore"
blockTypeName DiamondOre     = "Diamond Ore"
blockTypeName Snow           = "Snow"
blockTypeName Clay           = "Clay"
blockTypeName CraftingTable  = "Crafting Table"
blockTypeName Furnace        = "Furnace"
blockTypeName Chest          = "Chest"
blockTypeName Torch          = "Torch"
blockTypeName StoneBrick     = "Stone Brick"
blockTypeName Brick          = "Brick"
blockTypeName TNT            = "TNT"
blockTypeName Obsidian       = "Obsidian"
blockTypeName OakDoorClosed  = "Oak Door"
blockTypeName OakDoorOpen    = "Oak Door"
blockTypeName Ladder         = "Ladder"
blockTypeName Bed            = "Bed"
blockTypeName OakFence       = "Oak Fence"
blockTypeName Farmland       = "Farmland"
blockTypeName WheatCrop      = "Wheat"
blockTypeName OakSapling     = "Oak Sapling"
blockTypeName Wool           = "Wool"
blockTypeName FenceGateClosed = "Fence Gate"
blockTypeName FenceGateOpen  = "Fence Gate"
blockTypeName Lever          = "Lever"
blockTypeName RedstoneDust   = "Redstone Dust"
blockTypeName TrapdoorClosed = "Trapdoor"
blockTypeName TrapdoorOpen   = "Trapdoor"
blockTypeName StoneStairs    = "Stone Stairs"
blockTypeName OakStairs      = "Oak Stairs"
blockTypeName IronDoorClosed = "Iron Door"
blockTypeName IronDoorOpen   = "Iron Door"
blockTypeName Fire           = "Fire"
blockTypeName Cactus         = "Cactus"
blockTypeName SugarCane      = "Sugar Cane"
blockTypeName StoneSlab      = "Stone Slab"
blockTypeName OakSlab        = "Oak Slab"
blockTypeName Piston         = "Piston"
blockTypeName PistonHead     = "Piston Head"
blockTypeName Rail           = "Rail"
blockTypeName Dispenser      = "Dispenser"
blockTypeName EnchantingTable = "Enchanting Table"

toolTypeName :: ToolType -> String
toolTypeName Pickaxe = "Pickaxe"
toolTypeName Shovel  = "Shovel"
toolTypeName Axe     = "Axe"
toolTypeName Sword   = "Sword"
toolTypeName Hoe     = "Hoe"

toolMaterialName :: ToolMaterial -> String
toolMaterialName Wood      = "Wooden"
toolMaterialName StoneTier = "Stone"
toolMaterialName Iron      = "Iron"
toolMaterialName Diamond   = "Diamond"

armorMaterialName :: ArmorMaterial -> String
armorMaterialName LeatherArmor = "Leather"
armorMaterialName IronArmor    = "Iron"
armorMaterialName GoldArmor    = "Gold"
armorMaterialName DiamondArmor = "Diamond"

armorSlotName :: ArmorSlot -> String
armorSlotName Helmet     = "Helmet"
armorSlotName Chestplate = "Chestplate"
armorSlotName Leggings   = "Leggings"
armorSlotName Boots      = "Boots"

-- | Format an enchantment as "Name LEVEL" using roman numerals.
formatEnchantment :: Enchantment -> String
formatEnchantment (Enchantment etype level)
  | level <= 1 = enchantmentName etype
  | otherwise  = enchantmentName etype ++ " " ++ toRoman level

-- | Convert a small positive integer to a roman numeral string.
toRoman :: Int -> String
toRoman n
  | n <= 0    = ""
  | n >= 10   = "X" ++ toRoman (n - 10)
  | n >= 9    = "IX" ++ toRoman (n - 9)
  | n >= 5    = "V" ++ toRoman (n - 5)
  | n >= 4    = "IV" ++ toRoman (n - 4)
  | otherwise = "I" ++ toRoman (n - 1)

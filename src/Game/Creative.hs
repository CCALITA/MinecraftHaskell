module Game.Creative
  ( -- * Creative palette
    creativePalette
  , creativePaletteSize
    -- * Creative inventory click behavior
  , creativeClickSlot
  , creativePickFromPalette
    -- * Palette slot detection
  , palettePageCount
  , palettePageItems
  , hitPaletteSlot
    -- * Palette layout constants
  , paletteRows
  , paletteCols
  , paletteSlotsPerPage
  , paletteX0
  , paletteY0
  , paletteSlotW
  , paletteSlotH
  ) where

import Game.Inventory (Inventory, ItemStack(..), getSlot, setSlot)
import Game.Item
    ( Item(..)
    , ToolType(..)
    , ToolMaterial(..)
    , FoodType(..)
    , MaterialType(..)
    , ArmorSlot(..)
    , ArmorMaterial(..)
    , BucketType(..)
    , itemStackLimit
    )
import World.Block (BlockType(..))

-- ---------------------------------------------------------------------------
-- Creative palette: the canonical list of items available in creative mode
-- ---------------------------------------------------------------------------

-- | All items available in the creative palette.
-- Includes every placeable block type (except Air and technical blocks),
-- common tools, food, materials, and armor.
creativePalette :: [Item]
creativePalette = blockItems ++ toolItems ++ foodItems ++ materialItems ++ armorItems ++ miscItems
  where
    blockItems =
      [ BlockItem Stone
      , BlockItem Dirt
      , BlockItem Grass
      , BlockItem Sand
      , BlockItem Gravel
      , BlockItem OakLog
      , BlockItem OakLeaves
      , BlockItem Cobblestone
      , BlockItem OakPlanks
      , BlockItem Glass
      , BlockItem Bedrock
      , BlockItem IronOre
      , BlockItem CoalOre
      , BlockItem GoldOre
      , BlockItem DiamondOre
      , BlockItem Snow
      , BlockItem Clay
      , BlockItem CraftingTable
      , BlockItem Furnace
      , BlockItem Chest
      , BlockItem Torch
      , BlockItem StoneBrick
      , BlockItem Brick
      , BlockItem TNT
      , BlockItem Obsidian
      , BlockItem OakDoorClosed
      , BlockItem Ladder
      , BlockItem Bed
      , BlockItem OakFence
      , BlockItem Wool
      , BlockItem FenceGateClosed
      , BlockItem Lever
      , BlockItem RedstoneDust
      , BlockItem TrapdoorClosed
      , BlockItem StoneStairs
      , BlockItem OakStairs
      , BlockItem IronDoorClosed
      , BlockItem Cactus
      , BlockItem SugarCane
      , BlockItem StoneSlab
      , BlockItem OakSlab
      , BlockItem Piston
      , BlockItem Rail
      , BlockItem Dispenser
      , BlockItem EnchantingTable
      , BlockItem Netherrack
      , BlockItem SoulSand
      , BlockItem Glowstone
      , BlockItem NetherBrick
      , BlockItem Water
      , BlockItem Lava
      , BlockItem RedstoneOre
      , BlockItem LapisOre
      , BlockItem EmeraldOre
      , BlockItem MossyCobblestone
      , BlockItem MossyStoneBrick
      , BlockItem BirchLog
      , BlockItem BirchLeaves
      , BlockItem BirchPlanks
      , BlockItem SpruceLog
      , BlockItem SpruceLeaves
      , BlockItem SprucePlanks
      , BlockItem JungleLog
      , BlockItem JungleLeaves
      , BlockItem JunglePlanks
      , BlockItem Dandelion
      , BlockItem Rose
      , BlockItem BrownMushroom
      , BlockItem RedMushroom
      , BlockItem Bookshelf
      , BlockItem Anvil
      , BlockItem BrewingStand
      , BlockItem Ice
      , BlockItem PackedIce
      , BlockItem RedstoneLamp
      , BlockItem Hopper
      ]

    toolItems =
      [ ToolItem Pickaxe Wood 59
      , ToolItem Pickaxe StoneTier 131
      , ToolItem Pickaxe Iron 250
      , ToolItem Pickaxe Diamond 1561
      , ToolItem Axe Wood 59
      , ToolItem Axe StoneTier 131
      , ToolItem Axe Iron 250
      , ToolItem Axe Diamond 1561
      , ToolItem Shovel Wood 59
      , ToolItem Shovel StoneTier 131
      , ToolItem Shovel Iron 250
      , ToolItem Shovel Diamond 1561
      , ToolItem Sword Wood 59
      , ToolItem Sword StoneTier 131
      , ToolItem Sword Iron 250
      , ToolItem Sword Diamond 1561
      , ToolItem Hoe Wood 59
      , ToolItem Hoe Iron 250
      , ShearsItem 238
      , FlintAndSteelItem 64
      , FishingRodItem 64
      ]

    foodItems =
      [ FoodItem Apple
      , FoodItem Bread
      , FoodItem Steak
      , FoodItem CookedPorkchop
      , FoodItem CookedChicken
      , FoodItem CookedFish
      , FoodItem CookedSalmon
      , FoodItem RawBeef
      , FoodItem RawPorkchop
      , FoodItem RawChicken
      , FoodItem RawFish
      , FoodItem RawSalmon
      ]

    materialItems =
      [ MaterialItem Coal
      , MaterialItem DiamondGem
      , MaterialItem IronIngot
      , MaterialItem GoldIngot
      , MaterialItem Bone
      , MaterialItem ArrowMat
      , MaterialItem StringMat
      , MaterialItem Gunpowder
      , MaterialItem Feather
      , MaterialItem Leather
      , MaterialItem WheatSeeds
      , MaterialItem Wheat
      , MaterialItem Flint
      , MaterialItem Paper
      , MaterialItem LapisGem
      , MaterialItem Emerald
      , MaterialItem RedstoneDustMat
      , StickItem
      ]

    armorItems =
      [ ArmorItem Helmet LeatherArmor 55
      , ArmorItem Helmet IronArmor 165
      , ArmorItem Helmet DiamondArmor 363
      , ArmorItem Chestplate LeatherArmor 80
      , ArmorItem Chestplate IronArmor 240
      , ArmorItem Chestplate DiamondArmor 528
      , ArmorItem Leggings LeatherArmor 75
      , ArmorItem Leggings IronArmor 225
      , ArmorItem Leggings DiamondArmor 495
      , ArmorItem Boots LeatherArmor 65
      , ArmorItem Boots IronArmor 195
      , ArmorItem Boots DiamondArmor 429
      ]

    miscItems =
      [ CompassItem
      , ClockItem
      , BoatItem
      , MinecartItem
      , GlassBottleItem
      , BucketItem BucketEmpty
      , BucketItem BucketWater
      , BucketItem BucketLava
      ]

-- | Total number of items in the creative palette.
creativePaletteSize :: Int
creativePaletteSize = length creativePalette

-- ---------------------------------------------------------------------------
-- Palette layout constants (NDC coordinates)
-- ---------------------------------------------------------------------------

-- | Number of rows in the palette grid
paletteRows :: Int
paletteRows = 5

-- | Number of columns in the palette grid
paletteCols :: Int
paletteCols = 9

-- | Items per page in the palette
paletteSlotsPerPage :: Int
paletteSlotsPerPage = paletteRows * paletteCols

-- | Left edge of palette grid in NDC
paletteX0 :: Float
paletteX0 = -0.55

-- | Top edge of palette grid in NDC
paletteY0 :: Float
paletteY0 = -0.85

-- | Palette slot width in NDC
paletteSlotW :: Float
paletteSlotW = 0.10

-- | Palette slot height in NDC
paletteSlotH :: Float
paletteSlotH = 0.10

-- | Total number of palette pages
palettePageCount :: Int
palettePageCount =
  let total = creativePaletteSize
      perPage = paletteSlotsPerPage
  in if total == 0 then 1
     else (total + perPage - 1) `div` perPage

-- | Items on a given page (0-indexed)
palettePageItems :: Int -> [Item]
palettePageItems page =
  let start = page * paletteSlotsPerPage
  in take paletteSlotsPerPage (drop start creativePalette)

-- | Check if NDC coordinates hit a palette slot. Returns 0-indexed slot
-- within the current page.
hitPaletteSlot :: Float -> Float -> Maybe Int
hitPaletteSlot nx ny =
  let col = floor ((nx - paletteX0) / paletteSlotW) :: Int
      row = floor ((ny - paletteY0) / paletteSlotH) :: Int
  in if col >= 0 && col < paletteCols && row >= 0 && row < paletteRows
     then Just (row * paletteCols + col)
     else Nothing

-- ---------------------------------------------------------------------------
-- Creative inventory click behavior
-- ---------------------------------------------------------------------------

-- | Creative-mode click on a regular inventory slot.
-- Returns a full stack of whatever is in the slot (without consuming),
-- or places the cursor item as a full stack in the slot.
-- If the slot is empty and there is no cursor item, nothing happens.
creativeClickSlot :: Inventory -> Int -> Maybe ItemStack -> (Inventory, Maybe ItemStack)
creativeClickSlot inv slotIdx cursor =
  let slotContent = getSlot inv slotIdx
  in case (cursor, slotContent) of
    -- No cursor, slot has item: pick up a full stack (slot unchanged)
    (Nothing, Just (ItemStack item _count)) ->
      let fullStack = itemStackLimit item
      in (inv, Just (ItemStack item fullStack))
    -- Cursor has item, click on slot: place full stack in slot
    (Just (ItemStack item _), _) ->
      let fullStack = itemStackLimit item
          newInv = setSlot inv slotIdx (Just (ItemStack item fullStack))
      in (newInv, Nothing)
    -- No cursor, empty slot: nothing
    (Nothing, Nothing) ->
      (inv, Nothing)

-- | Pick an item from the creative palette. Always returns a full stack.
creativePickFromPalette :: Int -> [Item] -> Maybe ItemStack
creativePickFromPalette slotIdx pageItems
  | slotIdx >= 0 && slotIdx < length pageItems =
      let item = pageItems !! slotIdx
          fullStack = itemStackLimit item
      in Just (ItemStack item fullStack)
  | otherwise = Nothing

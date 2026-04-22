module Game.ItemDisplay
  ( itemColor
  , itemMiniIcon
  ) where

import Game.Item
import Game.Enchanting (Enchantment(..), EnchantmentType(..), enchantmentName)
import World.Block (BlockType(..))

itemColor :: Item -> (Float, Float, Float, Float)
itemColor (BlockItem bt) = case bt of
  Stone       -> (0.5, 0.5, 0.5, 1.0)
  Dirt        -> (0.55, 0.35, 0.17, 1.0)
  Grass       -> (0.2, 0.65, 0.1, 1.0)
  Sand        -> (0.82, 0.75, 0.5, 1.0)
  OakLog      -> (0.4, 0.3, 0.15, 1.0)
  OakPlanks   -> (0.78, 0.65, 0.43, 1.0)
  OakLeaves   -> (0.15, 0.5, 0.12, 1.0)
  Cobblestone -> (0.45, 0.45, 0.45, 1.0)
  Water       -> (0.15, 0.4, 0.8, 0.8)
  Lava        -> (0.9, 0.35, 0.05, 1.0)
  Torch       -> (0.95, 0.8, 0.2, 1.0)
  Glass       -> (0.7, 0.85, 0.95, 0.5)
  IronOre     -> (0.7, 0.6, 0.5, 1.0)
  CoalOre     -> (0.25, 0.25, 0.25, 1.0)
  GoldOre     -> (1.0, 0.85, 0.0, 1.0)
  DiamondOre  -> (0.3, 0.8, 0.95, 1.0)
  Snow        -> (0.95, 0.95, 0.95, 1.0)
  Obsidian    -> (0.12, 0.08, 0.16, 1.0)
  OakDoorClosed -> (0.6, 0.4, 0.2, 1.0)
  OakDoorOpen -> (0.6, 0.4, 0.2, 1.0)
  IronDoorClosed -> (0.7, 0.7, 0.7, 1.0)
  IronDoorOpen -> (0.7, 0.7, 0.7, 1.0)
  Ladder      -> (0.5, 0.35, 0.15, 1.0)
  Bed         -> (0.7, 0.15, 0.15, 1.0)
  OakFence    -> (0.55, 0.4, 0.2, 1.0)
  FenceGateClosed -> (0.55, 0.4, 0.2, 1.0)
  FenceGateOpen   -> (0.55, 0.4, 0.2, 1.0)
  TrapdoorClosed  -> (0.55, 0.4, 0.2, 1.0)
  TrapdoorOpen    -> (0.55, 0.4, 0.2, 1.0)
  Farmland    -> (0.35, 0.22, 0.1, 1.0)
  WheatCrop   -> (0.6, 0.7, 0.2, 1.0)
  OakSapling  -> (0.2, 0.55, 0.15, 1.0)
  Wool        -> (0.95, 0.95, 0.95, 1.0)
  Lever       -> (0.5, 0.5, 0.5, 1.0)
  RedstoneDust -> (0.8, 0.1, 0.1, 1.0)
  StoneStairs  -> (0.5, 0.5, 0.5, 1.0)
  OakStairs    -> (0.78, 0.65, 0.43, 1.0)
  Fire        -> (1.0, 0.5, 0.0, 1.0)
  Cactus      -> (0.2, 0.6, 0.2, 1.0)
  SugarCane   -> (0.4, 0.7, 0.3, 1.0)
  StoneSlab   -> (0.5, 0.5, 0.5, 1.0)
  OakSlab     -> (0.78, 0.65, 0.43, 1.0)
  Piston      -> (0.5, 0.4, 0.3, 1.0)
  PistonHead  -> (0.55, 0.45, 0.3, 1.0)
  Rail        -> (0.55, 0.45, 0.35, 1.0)
  Dispenser   -> (0.5, 0.5, 0.5, 1.0)
  EnchantingTable -> (0.3, 0.1, 0.4, 1.0)
  Netherrack       -> (0.5, 0.2, 0.2, 1.0)
  SoulSand         -> (0.35, 0.27, 0.2, 1.0)
  Glowstone        -> (0.9, 0.8, 0.4, 1.0)
  NetherBrick      -> (0.3, 0.12, 0.12, 1.0)
  NetherPortal     -> (0.5, 0.15, 0.8, 0.8)
  RedstoneOre      -> (0.7, 0.15, 0.15, 1.0)
  LapisOre         -> (0.15, 0.3, 0.7, 1.0)
  EmeraldOre       -> (0.2, 0.75, 0.3, 1.0)
  MossyCobblestone -> (0.35, 0.5, 0.35, 1.0)
  MossyStoneBrick  -> (0.4, 0.5, 0.4, 1.0)
  BirchLog         -> (0.8, 0.78, 0.72, 1.0)
  BirchLeaves      -> (0.3, 0.6, 0.25, 1.0)
  BirchPlanks      -> (0.85, 0.8, 0.65, 1.0)
  SpruceLog        -> (0.25, 0.18, 0.1, 1.0)
  SpruceLeaves     -> (0.12, 0.3, 0.15, 1.0)
  SprucePlanks     -> (0.5, 0.35, 0.2, 1.0)
  JungleLog        -> (0.5, 0.42, 0.25, 1.0)
  JungleLeaves     -> (0.18, 0.5, 0.12, 1.0)
  JunglePlanks     -> (0.65, 0.45, 0.3, 1.0)
  TallGrass        -> (0.25, 0.6, 0.15, 1.0)
  Dandelion        -> (1.0, 0.85, 0.2, 1.0)
  Rose             -> (0.85, 0.15, 0.15, 1.0)
  BrownMushroom    -> (0.55, 0.4, 0.25, 1.0)
  RedMushroom      -> (0.8, 0.15, 0.15, 1.0)
  Bookshelf        -> (0.55, 0.35, 0.2, 1.0)
  Anvil            -> (0.45, 0.45, 0.48, 1.0)
  BrewingStand     -> (0.5, 0.5, 0.55, 1.0)
  Ice              -> (0.6, 0.75, 0.9, 0.7)
  PackedIce        -> (0.5, 0.65, 0.85, 1.0)
  RedstoneLamp     -> (0.85, 0.6, 0.25, 1.0)
  Hopper           -> (0.4, 0.4, 0.42, 1.0)
  _           -> (0.6, 0.6, 0.6, 1.0)
itemColor (ToolItem Pickaxe _ _) = (0.7, 0.7, 0.8, 1.0)
itemColor (ToolItem Sword _ _)   = (0.8, 0.8, 0.9, 1.0)
itemColor (ToolItem Axe _ _)     = (0.6, 0.5, 0.3, 1.0)
itemColor (ToolItem Shovel _ _)  = (0.5, 0.4, 0.25, 1.0)
itemColor (ToolItem Hoe _ _)     = (0.5, 0.5, 0.3, 1.0)
itemColor StickItem              = (0.6, 0.4, 0.2, 1.0)
itemColor (FoodItem ft) = case ft of
  RawPorkchop    -> (0.9, 0.6, 0.6, 1.0)
  CookedPorkchop -> (0.7, 0.4, 0.2, 1.0)
  RawBeef        -> (0.9, 0.6, 0.6, 1.0)
  Steak          -> (0.7, 0.4, 0.2, 1.0)
  RawChicken     -> (0.9, 0.6, 0.6, 1.0)
  CookedChicken  -> (0.7, 0.4, 0.2, 1.0)
  Bread          -> (0.8, 0.7, 0.4, 1.0)
  Apple          -> (0.9, 0.2, 0.2, 1.0)
  RottenFlesh    -> (0.5, 0.6, 0.3, 1.0)
  RawFish        -> (0.7, 0.6, 0.65, 1.0)
  CookedFish     -> (0.8, 0.65, 0.35, 1.0)
  RawSalmon      -> (0.9, 0.5, 0.4, 1.0)
  CookedSalmon   -> (0.8, 0.55, 0.3, 1.0)
itemColor (MaterialItem mt) = case mt of
  Coal       -> (0.2, 0.2, 0.2, 1.0)
  DiamondGem -> (0.4, 0.9, 0.9, 1.0)
  IronIngot  -> (0.8, 0.8, 0.8, 1.0)
  GoldIngot  -> (0.9, 0.8, 0.3, 1.0)
  Bone       -> (0.9, 0.9, 0.85, 1.0)
  ArrowMat   -> (0.6, 0.55, 0.45, 1.0)
  StringMat  -> (0.9, 0.9, 0.9, 1.0)
  Gunpowder  -> (0.25, 0.25, 0.25, 1.0)
  Feather    -> (0.95, 0.95, 0.95, 1.0)
  Leather    -> (0.6, 0.35, 0.15, 1.0)
  WheatSeeds -> (0.3, 0.6, 0.2, 1.0)
  Wheat      -> (0.9, 0.8, 0.3, 1.0)
  Flint      -> (0.4, 0.4, 0.4, 1.0)
  Paper      -> (0.95, 0.95, 0.9, 1.0)
  LapisGem       -> (0.2, 0.3, 0.8, 1.0)
  Emerald        -> (0.2, 0.8, 0.3, 1.0)
  RedstoneDustMat -> (0.8, 0.1, 0.1, 1.0)
itemColor (ArmorItem _ mat _) = case mat of
  LeatherArmor -> (0.6, 0.35, 0.15, 1.0)
  IronArmor    -> (0.75, 0.75, 0.75, 1.0)
  GoldArmor    -> (0.9, 0.8, 0.3, 1.0)
  DiamondArmor -> (0.4, 0.9, 0.9, 1.0)
itemColor (ShearsItem _) = (0.7, 0.7, 0.7, 1.0)
itemColor (FlintAndSteelItem _) = (0.5, 0.5, 0.5, 1.0)
itemColor CompassItem = (0.7, 0.3, 0.3, 1.0)
itemColor ClockItem = (0.9, 0.8, 0.3, 1.0)
itemColor (FishingRodItem _) = (0.55, 0.35, 0.15, 1.0)
itemColor GlassBottleItem = (0.7, 0.85, 0.95, 0.6)
itemColor (PotionItem pt) = potionColor pt
itemColor BoatItem = (0.6, 0.4, 0.2, 1.0)
itemColor MinecartItem          = (0.6, 0.6, 0.6, 1.0)
itemColor (BucketItem bt) = case bt of
  BucketEmpty -> (0.7, 0.7, 0.7, 1.0)
  BucketWater -> (0.3, 0.5, 0.9, 1.0)
  BucketLava  -> (0.9, 0.4, 0.1, 1.0)
  BucketMilk  -> (0.95, 0.95, 0.9, 1.0)

-- | 3x3 mini-icon for item (row, col, color) — used in hotbar slot rendering
itemMiniIcon :: Item -> [(Int, Int, (Float, Float, Float, Float))]
itemMiniIcon (ToolItem Pickaxe _ _) =
  [(0,0,m),(0,1,m),(0,2,m), (1,1,s),(1,2,b), (2,0,b),(2,2,s)]
  where m = (0.7,0.7,0.8,1); s = (0.5,0.35,0.15,1); b = (0,0,0,0)
itemMiniIcon (ToolItem Sword _ _) =
  [(0,2,m), (1,1,m), (2,0,s)]
  where m = (0.8,0.8,0.9,1); s = (0.5,0.35,0.15,1)
itemMiniIcon (ToolItem Axe _ _) =
  [(0,1,m),(0,2,m), (1,0,s),(1,1,m), (2,0,s)]
  where m = (0.6,0.5,0.3,1); s = (0.5,0.35,0.15,1)
itemMiniIcon (ToolItem Shovel _ _) =
  [(0,1,m), (1,1,s), (2,1,s)]
  where m = (0.5,0.4,0.25,1); s = (0.5,0.35,0.15,1)
itemMiniIcon (ToolItem Hoe _ _) =
  [(0,1,m),(0,2,m), (1,1,s), (2,1,s)]
  where m = (0.5,0.5,0.3,1); s = (0.5,0.35,0.15,1)
itemMiniIcon StickItem =
  [(0,1,s), (1,1,s), (2,1,s)]
  where s = (0.6,0.4,0.2,1)
itemMiniIcon (FoodItem ft) = case ft of
  RawPorkchop    -> fill (0.9,0.6,0.6,1)
  CookedPorkchop -> fill (0.7,0.4,0.2,1)
  RawBeef        -> fill (0.9,0.6,0.6,1)
  Steak          -> fill (0.7,0.4,0.2,1)
  RawChicken     -> fill (0.9,0.6,0.6,1)
  CookedChicken  -> fill (0.7,0.4,0.2,1)
  Bread          -> [(1,0,c),(1,1,c),(1,2,c), (2,0,c),(2,1,c),(2,2,c)]
    where c = (0.8,0.7,0.4,1)
  Apple          -> [(0,1,g), (1,0,c),(1,1,c),(1,2,c), (2,1,c)]
    where c = (0.9,0.2,0.2,1); g = (0.3,0.6,0.2,1)
  RottenFlesh    -> fill (0.5,0.6,0.3,1)
  RawFish        -> fill (0.7,0.6,0.65,1)
  CookedFish     -> fill (0.8,0.65,0.35,1)
  RawSalmon      -> fill (0.9,0.5,0.4,1)
  CookedSalmon   -> fill (0.8,0.55,0.3,1)
  where fill c = [(r,col,c) | r <- [0..2], col <- [0..2]]
itemMiniIcon (MaterialItem mt) = case mt of
  Coal       -> [(0,1,c), (1,0,c),(1,1,c),(1,2,c), (2,1,c)]
    where c = (0.2,0.2,0.2,1)
  DiamondGem -> [(0,1,c), (1,0,c),(1,1,c),(1,2,c), (2,1,c)]
    where c = (0.4,0.9,0.9,1)
  IronIngot  -> [(0,0,c),(0,1,c),(0,2,c), (1,1,c),(1,2,c), (2,1,c),(2,2,c)]
    where c = (0.8,0.8,0.8,1)
  GoldIngot  -> [(0,0,c),(0,1,c),(0,2,c), (1,1,c),(1,2,c), (2,1,c),(2,2,c)]
    where c = (0.9,0.8,0.3,1)
  Bone       -> [(0,1,c), (1,1,c), (2,1,c)]
    where c = (0.9,0.9,0.85,1)
  ArrowMat   -> [(0,1,t), (1,1,s), (2,1,f)]
    where t = (0.5,0.5,0.5,1); s = (0.6,0.4,0.2,1); f = (0.9,0.9,0.9,1)
  StringMat  -> [(0,2,c), (1,1,c), (2,0,c)]
    where c = (0.9,0.9,0.9,1)
  Gunpowder  -> [(1,0,c),(1,1,c),(1,2,c), (2,1,c)]
    where c = (0.25,0.25,0.25,1)
  Feather    -> [(0,1,c), (1,0,c),(1,1,c), (2,1,c)]
    where c = (0.95,0.95,0.95,1)
  Leather    -> fill (0.6,0.35,0.15,1)
  WheatSeeds -> [(1,0,c),(1,2,c), (2,1,c)]
    where c = (0.3,0.6,0.2,1)
  Wheat      -> [(0,1,c), (1,0,c),(1,1,c),(1,2,c), (2,1,c)]
    where c = (0.9,0.8,0.3,1)
  Flint      -> [(0,1,c), (1,0,c),(1,1,c), (2,1,c),(2,2,c)]
    where c = (0.4,0.4,0.4,1)
  Paper      -> fill (0.95,0.95,0.9,1)
  LapisGem       -> [(0,1,c), (1,0,c),(1,1,c),(1,2,c), (2,1,c)]
    where c = (0.2,0.3,0.8,1)
  Emerald        -> [(0,1,c), (1,0,c),(1,1,c),(1,2,c), (2,1,c)]
    where c = (0.2,0.8,0.3,1)
  RedstoneDustMat -> [(1,0,c),(1,1,c),(1,2,c), (2,1,c)]
    where c = (0.8,0.1,0.1,1)
  where fill c = [(r,col,c) | r <- [0..2], col <- [0..2]]
itemMiniIcon (ArmorItem slot mat dur) = case slot of
  Helmet     -> [(0,0,c),(0,1,c),(0,2,c), (1,0,c),(1,2,c)]
  Chestplate -> [(0,0,c),(0,1,c),(0,2,c), (1,0,c),(1,1,c),(1,2,c), (2,0,c),(2,2,c)]
  Leggings   -> [(0,0,c),(0,1,c),(0,2,c), (1,0,c),(1,2,c), (2,0,c),(2,2,c)]
  Boots      -> [(1,0,c),(1,2,c), (2,0,c),(2,2,c)]
  where c = itemColor (ArmorItem slot mat dur)
itemMiniIcon (ShearsItem _) =
  [(0,0,c),(0,2,c), (1,1,c), (2,0,c),(2,2,c)]
  where c = (0.7,0.7,0.7,1)
itemMiniIcon (FlintAndSteelItem _) =
  [(0,2,fl), (1,1,g),(1,2,fl), (2,0,g),(2,1,g)]
  where g = (0.5,0.5,0.5,1); fl = (1.0,0.6,0.0,1)
itemMiniIcon CompassItem =
  [(0,1,r), (1,0,g),(1,1,g),(1,2,g), (2,1,g)]
  where r = (0.9,0.2,0.2,1); g = (0.7,0.7,0.7,1)
itemMiniIcon ClockItem =
  [(0,0,g),(0,1,g),(0,2,g), (1,0,g),(1,1,h),(1,2,g), (2,0,g),(2,1,g),(2,2,g)]
  where g = (0.9,0.8,0.3,1); h = (0.3,0.3,0.3,1)
itemMiniIcon (FishingRodItem _) =
  [(0,2,s), (1,1,s), (2,0,s),(2,2,st)]
  where s = (0.55,0.35,0.15,1); st = (0.9,0.9,0.9,1)
itemMiniIcon GlassBottleItem =
  [(0,0,gl),(0,2,gl), (1,0,gl),(1,1,gl),(1,2,gl), (2,1,gl)]
  where gl = itemColor GlassBottleItem
itemMiniIcon (PotionItem pt) =
  [(0,0,gl),(0,2,gl), (1,0,gl),(1,1,c),(1,2,gl), (2,1,c)]
  where gl = itemColor GlassBottleItem; c = potionColor pt
itemMiniIcon BoatItem =
  [(1,0,w),(1,2,w), (2,0,w),(2,1,w),(2,2,w)]
  where w = (0.6,0.4,0.2,1)
itemMiniIcon (BlockItem bt) = blockMiniIcon bt
  where
    fill c = [(r,col,c) | r <- [0..2], col <- [0..2]]
    blockMiniIcon Stone = fill (0.5,0.5,0.5,1)
    blockMiniIcon Dirt  = fill (0.55,0.35,0.17,1)
    blockMiniIcon Grass =
      [(0,0,g),(0,1,g),(0,2,g), (1,0,gs),(1,1,gs),(1,2,gs), (2,0,d),(2,1,d),(2,2,d)]
      where g = (0.2,0.65,0.1,1); gs = (0.35,0.55,0.2,1); d = (0.55,0.35,0.17,1)
    blockMiniIcon Sand = fill (0.82,0.75,0.5,1)
    blockMiniIcon OakLog =
      [(0,0,bk),(0,1,lt),(0,2,bk), (1,0,bk),(1,1,lt),(1,2,bk), (2,0,bk),(2,1,lt),(2,2,bk)]
      where bk = (0.4,0.3,0.15,1); lt = (0.6,0.5,0.25,1)
    blockMiniIcon OakPlanks = fill (0.78,0.65,0.43,1)
    blockMiniIcon OakLeaves = fill (0.15,0.5,0.12,1)
    blockMiniIcon Cobblestone =
      [(0,0,dk),(0,1,lt),(0,2,dk), (1,0,lt),(1,1,dk),(1,2,lt), (2,0,dk),(2,1,lt),(2,2,dk)]
      where dk = (0.35,0.35,0.35,1); lt = (0.5,0.5,0.5,1)
    blockMiniIcon Water = fill (0.15,0.4,0.8,0.8)
    blockMiniIcon Lava  = fill (0.9,0.35,0.05,1)
    blockMiniIcon Torch =
      [(0,1,fl), (1,1,st), (2,1,st)]
      where fl = (0.95,0.8,0.2,1); st = (0.5,0.35,0.15,1)
    blockMiniIcon Glass = fill (0.7,0.85,0.95,0.5)
    blockMiniIcon IronOre =
      [(0,0,st),(0,1,ore),(0,2,st), (1,0,st),(1,1,st),(1,2,ore), (2,0,ore),(2,1,st),(2,2,st)]
      where st = (0.5,0.5,0.5,1); ore = (0.7,0.6,0.5,1)
    blockMiniIcon CoalOre =
      [(0,0,st),(0,1,ore),(0,2,st), (1,0,ore),(1,1,st),(1,2,st), (2,0,st),(2,1,st),(2,2,ore)]
      where st = (0.5,0.5,0.5,1); ore = (0.15,0.15,0.15,1)
    blockMiniIcon GoldOre =
      [(0,0,st),(0,1,st),(0,2,ore), (1,0,ore),(1,1,st),(1,2,st), (2,0,st),(2,1,ore),(2,2,st)]
      where st = (0.5,0.5,0.5,1); ore = (1,0.85,0,1)
    blockMiniIcon DiamondOre =
      [(0,0,st),(0,1,ore),(0,2,st), (1,0,st),(1,1,ore),(1,2,st), (2,0,ore),(2,1,st),(2,2,ore)]
      where st = (0.5,0.5,0.5,1); ore = (0.3,0.8,0.95,1)
    blockMiniIcon Snow = fill (0.95,0.95,0.95,1)
    blockMiniIcon OakDoorClosed =
      [(0,0,w),(0,1,w),(0,2,w), (1,0,w),(1,1,w),(1,2,k), (2,0,w),(2,1,w),(2,2,w)]
      where w = (0.6,0.4,0.2,1); k = (0.3,0.25,0.2,1)
    blockMiniIcon OakDoorOpen =
      [(0,0,w), (1,0,w), (2,0,w)]
      where w = (0.6,0.4,0.2,1)
    blockMiniIcon _ = fill (itemColor (BlockItem bt))
itemMiniIcon item = fillSolid (itemColor item)
  where fillSolid c = [(r,col,c) | r <- [0..2], col <- [0..2]]

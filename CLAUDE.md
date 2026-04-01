# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build / Test / Run

```bash
# Build
cabal build

# Run (macOS — needs Vulkan loader on DYLD path)
DYLD_LIBRARY_PATH=/opt/homebrew/lib cabal run minecraft

# Run (Linux)
cabal run minecraft

# Tests (38 tests: HSpec + QuickCheck)
cabal test

# Compile shaders (only needed if .vert/.frag files change)
cd shaders && bash compile.sh && cd ..
```

**Prerequisites:** GHC >= 9.6, Cabal >= 3.0, Vulkan SDK or MoltenVK, GLFW3.
macOS: `brew install vulkan-loader molten-vk glfw`

## Architecture

**Entry point:** `app/Main.hs` — monolithic game loop using IORef-based state, no monad transformers.

**Rendering pipeline:** Vulkan with hand-managed resources (no VMA). Flow:
`Init → Swapchain → DepthBuffer → Pipeline(BlockVertex+UBO+Texture) → per-chunk CommandBuffer → DrawIndexed`

**Chunk system:** 16×16×256 blocks stored as `IOVector Word8` (mutable, O(1) read/write). Chunks managed in `TVar (HashMap ChunkPos Chunk)` with async generation. Per-chunk GPU mesh cache — only dirty chunks are re-meshed.

**Key data flow each frame:**
1. Poll GLFW input → build `PlayerInput`
2. Fixed-timestep physics at 20 Hz (player movement, collision)
3. Tick fluid simulation, entity AI, day/night cycle
4. Periodically load/unload chunks by player proximity
5. Update UBO (MVP matrices with `transpose` for row→column-major conversion, Vulkan clip correction for Y-flip and depth [0,1])
6. Record command buffer: bind pipeline, descriptor set, draw each chunk
7. Present swapchain image

## Key Conventions

- **StrictData** enabled globally — all fields strict by default
- **GHC2021** default language with 16 additional extensions (see `common-opts` in .cabal)
- Record field prefixes: `pl` (Player), `cam` (Camera), `vc` (VulkanContext), `sc` (Swapchain), `pc` (Pipeline), `bv` (BlockVertex), `ec` (EngineConfig)
- Vulkan qualified as `Vk`, GLFW as `GLFW`, vectors as `V`/`VS`/`UV`, HashMap as `HM`
- Manual `Storable` instances with explicit byte offsets for GPU data types
- `linear` library matrices are row-major; GLSL expects column-major → always `transpose` before GPU upload

## Module Map

| Subsystem | Modules | Status |
|-----------|---------|--------|
| **Renderer** | Engine.Vulkan.{Init,Swapchain,Pipeline,Command,Memory,Descriptor,Texture}, Engine.{Camera,Mesh,Types,Window} | Wired |
| **World** | World.{Block,Chunk,Noise,Biome,Generation,World} | Wired |
| **Simulation** | World.{Light,Fluid}, Game.DayNight | Wired |
| **Gameplay** | Game.{Physics,Player,Inventory,Save} | Wired |
| **Entities** | Entity.{ECS,Mob,Pathfinding,Spawn} | Wired (logic only, no rendering) |
| **Crafting** | Game.Crafting | Logic exists, UI not connected |
| **Redstone** | World.Redstone | Exists, NOT wired |
| **UI/HUD** | UI.{HUD,Menu,Text} | Exists, NOT wired (no text rendering pipeline) |
| **Networking** | Net.{Protocol,Server,Client} | Exists, NOT wired |

## Gotchas

- Negative world coordinates: use `divMod` (Haskell floor-division), not `div`/`mod` separately — the `worldToChunkLocal` function handles this correctly
- `meshChunkWithLight` freezes the mutable block vector for a consistent read snapshot during meshing
- Block shaders (`block_vert.spv`/`block_frag.spv`) must be recompiled if GLSL changes — the `.spv` files are gitignored
- macOS requires `DYLD_LIBRARY_PATH=/opt/homebrew/lib` and the `cabal.project` has macOS-specific Clang workaround for GLFW bindings

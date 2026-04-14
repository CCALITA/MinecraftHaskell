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

# Tests (43 tests: HSpec + QuickCheck)
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

**HUD pipeline:** Separate Vulkan pipeline for 2D overlay (no depth test, alpha blending). Renders crosshair, hotbar with item colors, health hearts, mining progress bar. Host-visible vertex buffer updated each frame.

**Chunk system:** 16×16×256 blocks stored as `IOVector Word8` (mutable, O(1) read/write). Chunks managed in `TVar (HashMap ChunkPos Chunk)` with async generation. Per-chunk GPU mesh cache — only dirty chunks are re-meshed.

**Key data flow each frame:**
1. Poll GLFW input → build `PlayerInput`
2. Fixed-timestep physics at 20 Hz (player movement, collision, fall damage)
3. Timed block mining (hold LMB, progress based on tool speed / block hardness)
4. Tick fluid simulation, entity AI, day/night cycle
5. Update dropped item physics, auto-collect nearby items
6. Periodically load/unload chunks by player proximity
7. Update UBO (MVP matrices with `transpose` for row→column-major conversion, Vulkan clip correction for Y-flip and depth [0,1], sun direction + ambient light)
8. Frustum cull chunks, record command buffer: draw visible chunks + HUD overlay
9. Present swapchain image

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
| **Items/Tools** | Game.{Item,DroppedItem,Crafting} | Wired (tools, durability, drop tables, timed mining) |
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
- `linear` library matrices are row-major; GLSL expects column-major → always `transpose` before GPU upload
- Vulkan NDC: Y=-1 is top, Y=+1 is bottom (opposite of OpenGL). The projection matrix applies a Y-flip correction, but HUD coordinates use raw Vulkan NDC.
- `isOnGround` uses 0.15 epsilon to handle collision resolution gap — player can float up to ~0.1 blocks above surface
- Camera yaw: mouse dx is *subtracted* from yaw (not added) because sin(yaw)/cos(yaw) with cross(front, up) gives -X as "right"

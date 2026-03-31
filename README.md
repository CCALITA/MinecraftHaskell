# Minecraft Haskell

A Minecraft clone written in Haskell with Vulkan rendering.

## Features

- **Vulkan Renderer** — GPU-accelerated voxel rendering with texture atlas, MVP matrices, and face culling
- **Infinite World** — Procedural terrain generation with Perlin noise, biomes (plains, mountains, desert, forest, tundra, swamp, jungle, mushroom, ocean), caves, ores, and trees
- **Chunk System** — 16x16x256 chunks with async loading/unloading based on player distance
- **Physics** — AABB collision detection, gravity, jumping, per-axis collision resolution
- **Block Interaction** — DDA raycasting (Amanatides & Woo) for break (LMB) and place (RMB)
- **Inventory** — 36-slot inventory with hotbar (keys 1-9), stack merging, block pickup on break
- **Crafting** — 2x2 and 3x3 grid-based recipe matching (10 recipes)
- **Day/Night Cycle** — 20-minute days, sky color interpolation, ambient light levels
- **Lighting** — BFS flood-fill for block light (torches, lava) and sky light propagation
- **Redstone** — Signal propagation with wires, torches, levers, repeaters, buttons
- **Fluids** — Water and lava flow simulation with source/flowing blocks and interaction (obsidian, cobblestone)
- **Entities** — ECS with 8 mob types (Zombie, Skeleton, Creeper, Spider, Pig, Cow, Sheep, Chicken), state-machine AI (Idle/Wander/Chase/Attack/Flee), A* pathfinding
- **Mob Spawning** — Light-level based hostile spawning at night, passive mobs on surfaces
- **Multiplayer** — TCP server/client with binary protocol (login, chunk data, entity updates, block changes)
- **Save/Load** — Binary serialization of world chunks and player state
- **UI System** — HUD (crosshair, hotbar, health/hunger bars, F3 debug overlay), menus (main, pause, settings), bitmap font text layout

## Controls

| Key | Action |
|-----|--------|
| WASD | Move |
| Mouse | Look |
| Space | Jump |
| Left Shift | Sneak |
| Left Ctrl | Sprint |
| F | Toggle fly mode |
| 1-9 | Select hotbar slot |
| LMB | Break block |
| RMB | Place block |
| ESC | Quit |

## Building

### Prerequisites

- [GHC](https://www.haskell.org/ghcup/) >= 9.6
- [Cabal](https://www.haskell.org/cabal/) >= 3.0
- [Vulkan SDK](https://vulkan.lunarg.com/) or MoltenVK (macOS)
- GLFW3

#### macOS (Homebrew)

```bash
brew install vulkan-loader molten-vk glfw
```

#### Linux (apt)

```bash
sudo apt install libvulkan-dev libglfw3-dev
```

### Build & Run

```bash
# Compile shaders (requires glslangValidator or glslc from Vulkan SDK)
cd shaders && bash compile.sh && cd ..

# Build
cabal build

# Run (macOS needs DYLD_LIBRARY_PATH for Vulkan loader)
# Linux:
cabal run minecraft

# macOS (Homebrew):
DYLD_LIBRARY_PATH=/opt/homebrew/lib cabal run minecraft
```

## Project Structure

```
├── app/
│   └── Main.hs                 # Entry point, game loop
├── src/
│   ├── Engine/
│   │   ├── Types.hs            # Shared types (vertex, UBO, config)
│   │   ├── Window.hs           # GLFW window management
│   │   ├── Camera.hs           # FPS camera, view/projection matrices
│   │   ├── Mesh.hs             # Block vertex, face-culled mesh generation
│   │   └── Vulkan/
│   │       ├── Init.hs         # Instance, device, queues
│   │       ├── Swapchain.hs    # Swapchain create/recreate
│   │       ├── Pipeline.hs     # Graphics pipeline, shaders
│   │       ├── Command.hs      # Command buffers, sync, draw
│   │       ├── Memory.hs       # Buffer allocation, staging uploads
│   │       ├── Descriptor.hs   # Descriptor sets, UBO binding
│   │       └── Texture.hs      # Texture atlas, image creation
│   ├── World/
│   │   ├── Block.hs            # 29 block types, properties, texture coords
│   │   ├── Chunk.hs            # 16x16x256 chunk storage (flat UVector Word8)
│   │   ├── Noise.hs            # Perlin noise (2D/3D, fractal, ridged)
│   │   ├── Biome.hs            # 9 biome types, temperature/humidity mapping
│   │   ├── Generation.hs       # Terrain gen (heightmap, caves, ores, trees)
│   │   ├── World.hs            # Chunk map, async loading, block get/set
│   │   ├── Light.hs            # BFS light propagation (block + sky)
│   │   ├── Redstone.hs         # Redstone signal graph, BFS propagation
│   │   └── Fluid.hs            # Water/lava flow simulation
│   ├── Game/
│   │   ├── Physics.hs          # AABB collision, gravity, ground check
│   │   ├── Player.hs           # Player state, input, DDA raycasting
│   │   ├── Inventory.hs        # 36-slot inventory, stack operations
│   │   ├── Crafting.hs         # Grid recipe matching
│   │   ├── DayNight.hs         # Time cycle, sky color, ambient light
│   │   └── Save.hs             # World/player serialization
│   ├── Entity/
│   │   ├── ECS.hs              # Entity storage, spawn/destroy/query
│   │   ├── Mob.hs              # Mob types, AI state machine
│   │   ├── Pathfinding.hs      # A* on voxel grid
│   │   └── Spawn.hs            # Spawn rules (light, surface, caps)
│   ├── Net/
│   │   ├── Protocol.hs         # Binary packet format (13 packet types)
│   │   ├── Server.hs           # TCP server, per-client threads
│   │   └── Client.hs           # TCP client, receive loop
│   └── UI/
│       ├── HUD.hs              # Crosshair, hotbar, health, debug overlay
│       ├── Menu.hs             # Menu screens, navigation
│       └── Text.hs             # Bitmap font layout
├── shaders/
│   ├── triangle.vert / .frag   # Basic vertex-colored shaders
│   ├── block.vert / .frag      # Textured block shaders with lighting
│   └── compile.sh              # GLSL → SPIR-V compilation
└── test/
    └── Spec.hs                 # Test suite
```

## Architecture

- **Fixed timestep**: Physics ticks at 20Hz (like Minecraft), rendering at vsync
- **Chunk storage**: Flat `UVector Word8` (65,536 bytes per chunk) for cache-friendly access
- **Concurrency**: STM for chunk map, `async` for background chunk generation
- **Collision**: Per-axis AABB resolution (Y → X → Z) to prevent corner sticking
- **Lighting**: Two-pass BFS — vertical sky light column trace, then horizontal spread
- **Networking**: Length-prefixed binary packets over TCP, server-authoritative

## License

MIT

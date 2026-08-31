# CLAUDE.md — mcg (Minecraft world/mission generator)

Guidance for Claude Code in this subtree. See the repo-root `CLAUDE.md` for monorepo layout.

## What this is

`mcg` is a C++17 static library that **procedurally generates Minecraft worlds and emits them
as JSON**. The JSON is consumed downstream by `WorldBuilder.java` in the Minecraft mod
(`external/malmo/Minecraft/.../edu/arizona/tomcat/Utils/WorldBuilder.java`), which actually
places the blocks. mcg never talks to Minecraft directly — its only output is JSON.

Deps: Boost (program_options) + nlohmann_json. Header API in `include/mcg/`, impl in `src/core/`.

## Build

- Standalone: `cd libs/mcg && mkdir build && cd build && cmake .. && make -j` → `lib/libmcg.a`.
- As part of ToMCAT: built automatically by the root `cmake ..`.
- `-DBUILD_EXAMPLES=ON` (OFF by default) builds the `examples/` tutorial world. From the repo
  root, `./tools/run_mcg_tutorial` builds-with-examples and runs it.

## Data model

A world is a tree of axis-aligned boxes containing blocks, mobs, objects, and connections.
Eight core classes (header / impl pair each):

- `Pos` — 3D integer point. **Coordinate convention: X/Z is the top-down plane, Y is height.**
- `Block` — a positioned block with a material string. `Door` extends it.
- `Entity` — a mob with optional equipment slots ([0]=helmet,[1]=chest,[2]=legs,[3]=boots,[4]=weapon).
- `Object` — a semantic wrapper owning a `Block` (gives blocks an id/type).
- `Connection` — a semantic doorway/transition linking location ids.
- `AABB` — **the central spatial container.** A cuboid defined by `topLeft`/`bottomRight`, with a
  material/type and flags (`isHollow`, `hasRoof`, `autoAdjust`). Holds child blocks, entities,
  objects, connections, **and nested AABBs.** Generation helpers: `generateBox`,
  `addRandomBlocks`, `generateAllDoorsInAABB`, `getRandomPos`, `getSubAABB`, `intersects`.
- `World` — top-level container of AABBs/blocks/entities/objects/connections; owns the
  `mt19937_64` RNG (seed via `setRandom`).

## Authoring pattern

Subclass `World` and `AABB` (see `examples/mcg_tutorial/mcg_tutorial.cpp`: a `Room : AABB` sets
material/floor/windows/roof + a random mob; a `TutorialWorld : World` nests two rooms in a parent
"house" AABB with `autoAdjust`). Then `World::writeToFile(semanticPath, lowLevelPath)` emits **two
JSON files**: a high-level *semantic* map (locations + bounds + connections) and a *low-level* map
(every block/entity enumerated). Both come from virtual `toSemanticMapJSON`/`toLowLevelMapJSON`
on each class.

## Conventions / gotchas

- **Ownership:** every `add*` takes a `unique_ptr` and `std::move`s it in. Don't keep a raw
  pointer after adding.
- **`autoAdjust`:** the default `AABB(id)` ctor makes a "blank canvas" (material `"blank"`,
  `autoAdjust=true`) that grows to fit its children — the idiom for container AABBs. Material
  `"blank"` is special: `WorldBuilder.java` skips it, so it places nothing.
- `topLeft` is the min corner, `bottomRight` the max (incl. Y low→high).

## Visualizer (`mcgviz/`)

Python 3 (matplotlib + numpy + pygraphviz). Reads the *semantic* JSON and renders a top-down
spatial plot (`map_plot.pdf`) and a parent/child hierarchy graph (`map_graph.pdf`). Flags:
`--color_patches`, `--font_size`, `--background`, `--rankdir`, etc.

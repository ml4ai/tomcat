# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

ToMCAT (Theory-of-Mind based Cognitive Architecture for Teams) is a research project at
the University of Arizona. It is a **polyglot monorepo**, not a single application. Three
largely independent worlds coexist; identify which one a task touches before doing anything:

1. **C++ core** (`libs/`, `exe/`) — built with CMake. The headline library is `libs/mcg`
   (Minecraft world/mission *procedural generation*). Executables in `exe/` consume it:
   `runMission`, `mcgen` (procedural generator), `getSpecVersion`, `cpp_agent_template`.
   See `libs/mcg/CLAUDE.md` for the generator library's data model and JSON output.
2. **Minecraft mod** (`external/malmo/`) — a vendored copy of Microsoft Malmo plus the
   project's own Java mod under `external/malmo/Minecraft/src/main/java/edu/arizona/tomcat/`.
   Built with Gradle, driven through CMake as the `Minecraft` target.
3. **Human experiments apparatus** (`human_experiments/`) — the lab data-collection and
   data-processing system. This is where nearly all recent work happens (see git log and
   the `orca_db` branch). Mostly Python and Bash, with its own build/run conventions that
   have **nothing to do with the top-level CMake build**.

Rendered docs: https://ml4ai.github.io/tomcat (source in `docs/`, Sphinx + Doxygen).

## Building the C++ / Minecraft core

```bash
./tools/install          # clone-to-running: installs deps, then cmake + make + make Minecraft
```

`tools/install` shells out to `tools/install_dependencies` (Boost, libfmt, OpenCV, dlib,
Mosquitto, Gradle, ffmpeg, etc.) and downloads a pinned OpenJDK 8 before building. Inspect
those scripts rather than guessing at dependency versions.

Manual build (after deps are present):

```bash
mkdir build && cd build
cmake ..                 # add -DBUILD_EXAMPLES=ON to build mcg tutorial worlds
make -j                  # C++ targets
make -j Minecraft        # the Gradle-built Minecraft mod
```

- C++17 is required (`CMAKE_CXX_STANDARD 17`). Build artifacts land in `build/bin` and `build/lib`.
- `-DBUILD_DOCS=ON` enables the `docs` subdirectory; docs are normally built separately (see CI).
- mcg can be built standalone: `cd libs/mcg && mkdir build && cd build && cmake .. && make -j`.
- Tutorial world: `./tools/run_mcg_tutorial` (requires a build done with `-DBUILD_EXAMPLES=ON`).

## human_experiments/ — the lab system

This subtree does not participate in the root CMake build. Key areas:

- `scripts/run_experiment` — the **uber-script** that drives a full in-lab experiment as a
  numbered sequence of steps (testbed, Minecraft, baseline tasks, LSL/LabRecorder, audio/video
  capture, data inventory). Resume at a step with `GOTO=<n> ./run_experiment`. Config lives in
  `config/uber_script/config`. The other files in `scripts/` are individual steps it invokes.
- `dataset_pipeline/` — the data pipeline: raw experiment files → Postgres cluster →
  bulk-download artifacts, driven by `make`. Self-contained (Python, not CMake).
  **See `human_experiments/dataset_pipeline/CLAUDE.md`** for its build, the dev/prod switch,
  and architecture. Datasette-era leftovers are parked in its `legacy_datasette/`.
- `dataset_website/` — the public FastAPI site (`tomcat.lab.pyarelal.xyz`) that serves the
  Postgres dataset read-only. Self-contained; deployed as a systemd unit on orca.
  **See `human_experiments/dataset_website/CLAUDE.md`.**
- `lab_software/` — many **standalone** capture/viz/extraction tools (video/audio capture,
  physio viz, data_inventory, visualizer, baseline-tasks, etc.). Each is its own mini-project
  with its own `requirements.txt`/`README`; do not assume shared dependencies.
  **See `human_experiments/lab_software/CLAUDE.md`** for the per-tool catalog and the LSL/MQTT buses.
- `signal_filtering/` and `synchronize_signal_task/` — the signal-processing stages used by
  the dataset_pipeline `sync_*` step (read → filter → synchronize → write).

## Formatting & conventions

- C++ and Java are formatted with **clang-format** (`.clang-format`: 4-space indent, left
  pointer alignment, custom brace wrapping). Run `tools/dev/autoformat_code [dir]` — it
  clang-formats both the Java mod sources and C++ sources (requires GNU `parallel`).
- Python under `dataset_pipeline/` and `dataset_website/` uses **ruff** (`make lint` = `ruff check --fix && ruff format`).
- There is no project-wide automated test suite; "tests" found in the tree are ad-hoc
  per-tool scripts (e.g. `*_latency_test`, `tomcat-physio-viz/test.py`), not a unified runner.

## CI

The only GitHub Actions workflow (`.github/workflows/deploy.yml`) builds the Sphinx/Doxygen
docs and publishes `docs/build` to GitHub Pages on push to `master`. It does **not** build or
test the C++/Minecraft/Python code — that build correctness is not gated by CI.

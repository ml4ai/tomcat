# CLAUDE.md — lab_software

Guidance for Claude Code in this subtree. See the repo-root `CLAUDE.md` for monorepo layout.

## What this is

A collection of **independent, standalone tools** used to run and capture data during
in-lab experiments. There is **no shared build, no shared `requirements.txt`, no shared
config** — each subdirectory is its own mini-project. Read the specific tool's own
`README`/launcher before changing it; do not assume conventions carry across tools.

The C++ tools each build with their own CMake (`mkdir build && cd build && cmake .. && make -j`,
output in `build/bin/`) and link LSL/Boost/Mosquitto via `../../tools/cmake/`. The Python and
Bash tools are run directly, often via a menu-driven launcher script.

## The two integration buses

Most tools plug into one of two real-time buses — this is the main thing tying them together:

- **LSL (Lab Streaming Layer)** — the primary data bus for physio/audio/video/event streams
  (stream names like `Audio`, `Webcam`, `Screen`, `Minecraft`, `USBAudioStream`, plus EEG/fNIRS
  device streams). Recordings are saved as `.xdf` files.
- **MQTT** — Minecraft/testbed event messages; bridged into LSL by `mqtt_to_lsl`.

Lab workstations are referred to by name (e.g. `lion`, `tiger`, `leopard`) in the tools that
fan out over SSH (`say_announcements`, `monitor_programs`).

## Tool catalog

Capture (C++ unless noted):
- `audio_capture` — mic → WAV + LSL `Audio` stream.
- `video_capture` — webcam/screen frames (OpenCV/ffmpeg) → LSL, UTC-timestamped.
- `global_mic` — USB mic → LSL `USBAudioStream`/WAV (Python, sounddevice+pylsl);
  `list_lsl_streams.py` here is the handy LSL connectivity debugger.
- `mqtt_to_lsl` — MQTT (Minecraft events) → LSL `Minecraft` stream.
- `tomcat-images-timestamp` — watches a dir, emits file-creation timestamps (stdout/file/MQTT).

Experiment tasks / timing (Python):
- `tomcat-baseline-tasks` — networked task suite (rest, finger-tapping, affective, ping-pong);
  pygame+asyncio+LSL. Run `run_server.py` then a `run_client*.py` per participant.
- `tomcat-time-difference` — measures end-to-end latency across networked clients.

Physio extraction & viz (Python):
- `tomcat-physio-viz` *(active)* — live EEG/fNIRS plots (PyQt5+pyqtgraph+pylsl); launch via
  `./run_physio_viz.sh`.
- `tomcat-physio-data-extraction` — XDF → CSV/HDF5/pickle (older pipeline; separate
  baseline+minecraft dirs). `data_extraction` is a legacy symlink to it.
- `tomcat-physio-data-extraction_v2` — simpler single-XDF-dir extractor (post-2023-04-17 data).

Orchestration / utilities (mostly Bash, menu-driven — pass `-h`):
- `data_inventory` *(active)* — audits an experiment directory against a definition file;
  `./data_inventory.sh` (supports CLI flags for exclusions/reports).
- `visualizer` *(active)* — Node.js web viewer for face/screen images; `./visualizer.sh`.
- `monitor_programs` — TUI dashboard of task/minecraft/physio/capture status across workstations.
- `say_announcements` — TTS announcements pushed to workstations over SSH (`./say_announcement_menu`).
- `combine_presession_data` — symlinks pre-session participant data into the experiment dir.
- `export_redcap_data` — pulls REDCap survey data via API (`./export_redcap_menu`).

The three marked *(active)* are where recent work has been concentrated (see git log).

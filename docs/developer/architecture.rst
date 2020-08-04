Design and Architecture
=======================

Building upon Project Malmo
---------------------------

ToMCAT extends `Project Malmo` to provide a platform to study human behavior in
a virtual environment, with a focus on studying human-human and human-machine
teaming.

The core of Project Malmo consisted of two components: a static C++ library and
a Java mod for Minecraft. Together, these components exposed an API for
communicating with Minecraft over a TCP socket, thus enabling researchers to
programmatically control avatars in the Minecraft world and declaratively
specify 'missions' that provided suitable environments for reinforcement
learning research.

ToMCAT inverts the original research direction of Project Malmo somewhat -
rather than serving as a reinforcement learning research platform to teach AI
agents to perform complex tasks in Minecraft, we focus on learning about how
*humans* think and behave. We have vendorized the source of Project Malmo
(``external/malmo``) and modified it to suit our purposes:
1. The original Java mod (``external/malmo/Minecraft``) has been extended to
   - allow human control by default
   - add software instrumentation to capture human actions in the Minecraft
     environment.
   - add 'missions' (and documentation on how to implement new ones) to support
     developing machine social intelligence.
2. The original C++ code for the Malmo static library has not been changed
   significantly (besides a bit of modernization and code formatting). We use
   this library to create an executable, ``runMission`` that serves as the driver
   for our experiments.

There are a couple of other high-level differences in the technical approaches taken
by Malmo and ToMCAT.

* In Malmo, a mission environment can be declaratively specified using low
  (e.g. ``DrawBlock``, ``DrawEntity`` elements) and high (e.g.
  ``BuildBattleDecorator``) level XML elements, and corresponding 'decorator'
  implementations on the Java side.  In contrast, the equivalent functionality
  in ToMCAT has been almost completely extracted out into a separate C++ module
  (src/cpp/pro_gen) that simultaneously creates low-level (block and entity
  positions, types, etc) and high-level (specifications of bounding volumes and
  topological structure) machine-readable representations of the mission
  environment that can be then parsed relatively straightforward on the Java
  side to create the mission environment. These representations can also be
  used downstream by other applications, including AI agents in the ASIST
  program.

* Malmo focuses on state observations at regular time intervals, but when
  studying humans, it is desirable to record events that do not necessary
  happen at regular time intervals (e.g. opening a door, flipping a lever,
  attacking a mob). These kinds of events are accessible through the Forge API,
  and ToMCAT exposes a subset of these (see the `documentation on events and
  data models`_ and `instructions on how to implement new missions and events`_
  for more details).

Automation
----------

We place a heavy premium on automation, and thus eschew support for Windows and
non-LAN multiplayer in favor of maintaining a set of robust scripts (in the
``tools`` directory) and a procedural generation module (``src/cpp/pro_gen``)
to ease the lives of developers and end-users by minimizing the amount of
complex documentation they need to read to get set up. Our software is
currently tested on macOS and Ubuntu with a continuous integration pipeline.

Sensors
-------

Audio and Video
^^^^^^^^^^^^^^^

We implement sensors to record video and audio of the human player and the game
itself. The video and audio of the player and the video of the game are
captured using `ffmpeg`_, and the audio of the game is captured using `pacat`_
(Linux) or `BlackHole`_ (macOS).

In-game observations and events
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We implement capturing in-game observations and events related to the human
players and the missions they are conducting. These are published to an MQTT
message bus on various topics.

Facial Landmark, Gaze, Pose, and AU detection
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We leverage `OpenFace`_ to create an executable (``faceSensor``, whose code
resides in ``src/cpp/faceSensor``) that outputs JSON messages containing
information about a player's gaze, pose, action units, and face landmarks,
either from a webcam feed or from a video file. These messages are output to
standard output, from where they can be either redirected to a file, piped into
an MQTT client (like ``mosquitto_sub``) for publication, or used for other
downstream applications.

Note: We vendorize OpenFace under ``external/OpenFace`` since we have made some
modifications to it.

.. _documentation on events and data models: ../tomcat_openapi.html
.. _instructions on how to implement new events: missions.html
.. _Project Malmo: https://github.com/microsoft/malmo
.. _ffmpeg: http://ffmpeg.org
.. _pacat: https://linux.die.net/man/1/pacat
.. _BlackHole: https://github.com/ExistentialAudio/BlackHole
.. _OpenFace: https://github.com/TadasBaltrusaitis/OpenFace

Guide to developing ToMCAT missions
===================================

ToMCAT aims to provide a flexible framework for studying human-machine
teaming in the virtual world of Minecraft. It provides an API for constructing
various scenarios in Minecraft that can act as focused tests of specific
aspects of human-machine teaming as well as machine social intelligence.
Following the parlance of [Project Malmo](https://github.com/microsoft/malmo),
(the platform we build upon), we term these scenarios as 'missions'.

In this section, we will go over how to design and implement a ToMCAT mission.
All file and directory paths are relative to the root of the `ml4ai/tomcat`
repo.

Building the environment
------------------------

You have a couple of options when it comes to setting up the mission
environment.
- The first is to use a [manually-designed and
  built](https://minecraft.gamepedia.com/Tutorials/Menu_screen#Creating_a_New_World)
  (by yourself or someone else) world. Then, specify the full path to the world
  folder and the starting position of the human player in the `src` attribute
  `Mission.ServerSection.ServerHandlers.FileWorldGenerator` section of a
  Malmo-style XML specification. See `conf/xml_mission_specs/file_world.xml`
  for an example of this approach.
- The other option is to use ToMCAT's `pro_gen` procedural generation module
  (see `src/cpp/pro_gen` for the source code and `tools/run_session` for
  example usage) to have more control over the generated mission environment.
  In an experiment context, it is usually desirable to have parametric control
  over the experimental conditions and stochasticity, both of which can be
  achieved with procedural generation.

Setting up the mission skeleton
-------------------------------

After building the mission environment as described above, the next step is to
define the mission behavior, - for example, the mission time limit, the
behaviour of mobs, and handling in-game events of interest.

### Java

On the Java side, we will create some scaffolding code in the folder
`external/malmo/Minecraft/src/main/java/edu/arizona/tomcat/Mission`.

To create a new mission called do the following steps outlined below. We'll
call this mission `ExampleMission`, and all the filepaths specified will be
under the `external/malmo/Minecraft/src/main/java/edu/arizona/tomcat/Mission`
folder. We will not go into all the details of the classes, but will point the
reader to the other missions in that directory (e.g. `TutorialMission`,
`ZombieMission`, `ProceduralGenMission`) as examples to follow.

- Add a class called `ExampleMission.java`. Specify the player's initial
  position in the `getPlayersInitialPositionAndDirection` method.
- Add a member to the `ID` enum in `Mission.java` for the example mission.
- Add a case in the switch statements for the `create` and `createClient`
  methods in `MissionFactory.java` for the example mission.
- Create a class `Client/ExampleClientMission.java`

### C++

If you want to be able to run the mission using the `tools/run_session` script
in order to automatically set up A/V recording and the message bus, you'll need
to add some code to the C++ side as well.

- In `src/cpp/Mission.h`:
  - Add a member to the MissionId enum.
  - Update `id_to_world_folder_map` with the location of the world folder under
    `data/worlds`.
- Update the description of the `--mission` command-line argument
  `src/cpp/runExperiment.cpp` with a short description of `ExampleMission`.
- Add a line to the comments in `tools/configuration_helpers` describing the
  `MAIN_MISSION` environment variable about `ExampleMission`.

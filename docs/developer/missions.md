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
  `src/cpp/runMission.cpp` with a short description of `ExampleMission`.
- Add a line to the comments in `tools/configuration_helpers` describing the
  `MAIN_MISSION` environment variable about `ExampleMission`.

Defining event handlers
-----------------------

Given that you are implementing a custom mission, it's likely that there are
certain in-game events that you will want to (i) record for downstream
analysis, (ii) trigger other in-game events, or both.

Events are recorded using JSON messages published to an MQTT message bus. In
order to ensure valid JSON and reduce duplication of code, we define Java
classes for the different types of events and use the
[Gson](https://github.com/google/gson) library to automatically serialize
instances of those classes to JSON. The inheritance hierarchy of the classes
helps us avoid code duplication for event types that are related by
hypernymy/hyponymy, and thus share certain data fields.

The event classes reside in the
`external/malmo/Minecraft/src/main/java/edu/arizona/tomcat/Events` folder. All
event classes inherit from the `Event` class, which has only two basic
attributes that are shared by all events: `eventType` and `timestamp`.

In order to implement a new type of event, create a new Java class that
inherits from `edu.arizona.tomcat.Event`. The event should define private data
members that will be serialized by Gson. The constructor of the event may or
may not take an argument. Most of the implement event classes take one
argument, i.e. one of the event types that are published by
[Forge](https://files.minecraftforge.net). The [Forge API
documentation](https://skmedix.github.io/ForgeJavaDocs/javadoc/forge/1.11.2-13.20.0.2228/)
contains descriptions of all the possible events that can be subscribed to, as
well as the descriptions of the block and entity types involved in the event.

Once you've created the event class, you can implement a handler for the event
in either your mission class (e.g. in `ExampleMission.java`), or in the
`ForgeEventHandler.java` class. Most events that have been implemented so far
are handled in the `ForgeEventHandler` class, which is then utilized in the
actual `*Mission.java` class files, since they are fairly general and not
specific to a particular mission.

For an example of handling a mission-specific event, see the `PlayerDeath`
handler method in `Mission.java`. For examples of non mission-specific event
handlers, see the `ForgeEventHandler` class.

If you are implementing an event that needs to be published to the message bus
(this is most likely the case), then you'll need to use the
[`edu.arizona.tomcat.Messaging.MqttService`](https://ml4ai.github.io/tomcat/developer/java_api/classedu_1_1arizona_1_1tomcat_1_1Messaging_1_1MqttService.html) singleton class. The class has an
overloaded method, `publish`, which can publish either an object or a string -
in the case of an object, it is automatically serialized to Gson. The `publish`
method also takes a second string-type argument, which is the topic that the
message corresponding to the event should be published to. You can see the
currently implemented topics and message data models
[here](https://ml4ai.github.io/tomcat/tomcat_openapi.html).

If you add an event that corresponds to a published message, you'll also need
to add the message schema and topic for that event to `docs/spec.yml`,
following the pattern of the other messages and topics that are already in
there.

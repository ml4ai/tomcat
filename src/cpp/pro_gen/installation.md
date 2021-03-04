## Prerequisites

* Cmake 3.10 or above 
* Boost 1.69
* Python 3.7+ for the vizualiser

## Installation Steps

The following steps are to install the procedural generation library only. Details about the Minecraft Java code that reads the JSON procduced by the library is given at the end of the document.

* Clone this [ToMCAT repository](https://github.com/ml4ai/tomcat/tree/master.)
* Navigate into `src/cpp/pro_gen/`.
* Open a terminal prompt in this folder.
* Create a directory called `build` and go into it with `mkdir build && cd build`.
* Run `cmake ..`.
* Finally, run `make -j`.

That's it! There should now be an executable in that folder called `generator`.


## Tips on Running the Generator

The following assumes you're still inside the `build` directory from before or in the same directory as where `generator` is

* Running `./generator`  should generate the ZombieWorld by default.
* Running `./generator -h` or `./generator --help` should list the various program options available.
    *  You should see options to set the seed for the random object (which may or may not be relevant depending on if you use the random object in your algorithm).
    *  You should also see options to specify where to output the low level and semantic JSONs. Of the two, the Minecraft Java code reads the low level JSON, and the semantic JSON is meant to be more human readable.
    * The `world_type` program option allows you to choose whether you want to generate the Gridworld or ZombieWorld using algorithms pre-included in the library. The Gridworld type supports more options which you can pass. You can look at what these additional options are with `./generator --help_gridworld`.


## Minecraft Java Code

In ToMCAT, a class called `WorldBuilder` was created to read and index everything to place from the library's procedural generator output. The [class can be found here](https://github.com/ml4ai/tomcat/blob/master/external/malmo/Minecraft/src/main/java/edu/arizona/tomcat/Utils/WorldBuilder.java). 

Using this class is quite simple. You would simply do:

    private void buildStructures(World world) {
            WorldBuilder worldBuilder = new WorldBuilder(); // Create a world builder object
            worldBuilder.build("low_level_map.json", world, false, true); // Send it the world and the JSON to build with
    }

Here, "low_level_map.json" is the low level JSON output from the library, and world is a Minecraft world as represented in Minecraft's code. The false and true arguments specify wheter the WorldBuilder object should save the indexes it creates. Saving them can be fairly memory intensive.

[An example of it's use in ToMCAT can be found here.](https://github.com/ml4ai/tomcat/blob/master/external/malmo/Minecraft/src/main/java/edu/arizona/tomcat/Mission/ProceduralGenMission.java)

## Creating your first algorithm

A more detailed tutorial will be added. Examples of algorithms can be found in `GridWorld.cpp` and `ZombieWorld.cpp`.

Decide on an algorithm/file name and then add a line to `CmakeLists.txt` after `Gridworld.cpp` as `<your file name>.cpp`. Add it before the closing parenthesis.
Also remember to add a program option for your new algorithm in `generator.cpp`.

When you're ready to see the JSON output, navigate into the `build` directory again, and run `cmake .. && make -j`. The `generator` executable should be created again which you can now use with your new program option.

## Vizualising the Map

There is a small python script included in the `pro_gen` library that can vizualise the maps you create with the library. Place the low level JSON file named as `low_level_map.json` in the same directory as the `pro_gen_vizualiser.py` script. Run the script to see the map vizualised in `vizualized_plt.png`.

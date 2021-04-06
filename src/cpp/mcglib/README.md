## Library Prerequisites
* Cmake 3.10 or above 
* Boost 1.69
* nlohmann_json

## Visualizer Prerequisites
* Python 3.7+
* matplotlib (3.3.3 was used in development)
* numpy (1.19 was used in development)
* pygraphviz (1.7 was used in development)

## Installation Steps

The following steps are to install mcglib only. Details about the Minecraft
Java code that reads the JSON produced by the library is given later in the
document.

* Clone this [ToMCAT repository](https://github.com/ml4ai/tomcat/tree/master.)
* Navigate into `src/cpp/mcglib/`.
* Open a terminal prompt in this folder.
* Create a directory called `build` and go into it with `mkdir build && cd build`.
* Run `cmake ..`.
* Finally, run `make -j`.

That's it! There should now be an executable in that folder called `generator`.


## Tips on Running the Generator

The following assumes you're still inside the `build` directory from before or
in the same directory as the `generator` executable.

* Running `./generator`  should generate the ZombieWorld by default.
* Running `./generator -h` or `./generator --help` should list the various program options available.
    * You should see options to set the seed for the random object (which may
      or may not be relevant depending on if you use the random object in your
      algorithm).
    * You should also see options to specify where to output the low level and
      semantic JSONs. Of the two, the Minecraft Java code reads the low level
      JSON, and the semantic JSON is meant to be more human readable.
    * The `world_type` program option allows you to choose whether you want to
      generate the GridWorld,ZombieWorld, or DungeonWorld using algorithms
      pre-included in the library. The GridWorld type supports more options
      which you can pass. You can look at what these additional options are
      with `./generator --help_gridworld`.


## Minecraft Java Code

In ToMCAT, a class called `WorldBuilder` was created to read and index
everything to place from the library's procedural generator output. The [class
can be found
here](https://github.com/ml4ai/tomcat/blob/master/external/malmo/Minecraft/src/main/java/edu/arizona/tomcat/Utils/WorldBuilder.java). 

Using this class is quite simple. You would simply do:

```java
private void buildStructures(World world) {
        WorldBuilder worldBuilder = new WorldBuilder(); // Create a world builder object
        worldBuilder.build("low_level_map.json", world, false, true); // Send it the world and the JSON to build with
}
```

Here, "low_level_map.json" is the low level JSON output from the library, and
world is a Minecraft world as represented in Minecraft's code. The false and
true arguments specify whether the WorldBuilder object should save the indexes
it creates. Saving them can be fairly memory intensive.

[An example of it's use in ToMCAT can be found here.](https://github.com/ml4ai/tomcat/blob/master/external/malmo/Minecraft/src/main/java/edu/arizona/tomcat/Mission/ProceduralGenMission.java)

## Creating your first algorithm

A more detailed tutorial will be added. Examples of algorithms can be found in
`GridWorld/GridWorld.cpp`, `ZombieWorld/ZombieWorld.cpp`,
`DungeonWorld/DungeonWorld.cpp`.

Decide on an algorithm/file name and then add a line to `CmakeLists.txt` after
`${mcglib_dungeonworld}` as `<your file name>.cpp`. Add it before the closing
parenthesis.
Also remember to add a program option for your new algorithm in `generator.cpp`.

When you're ready to see the JSON output, navigate into the `build` directory
again, and run `cmake .. && make -j`. The `generator` executable should be
created again which you can now use with your new program option.

## Visualizing the Map

There is a small python script included in the `mcglib` library that can
visualize the maps you create with the library. Assuming you've installed the
required dependencies listed at the top of this file:

* Place the semantic JSON file named as `semantic_map.json` in the same directory as the `mcg_visualizer.py` script. 
* Run the script with `python3 mcg_visualizer.py` 
* See the plot in `map_plot.pdf` and the graph in `map_graph.pdf`. 

There are also various program options available for the visualizer. You can
learn about them by running `python3 mcg_visualizer.py -h` or `python3
mcg_visualizer.py --help`.

## JSON Spec

For each algorithm, the library outputs two JSON representations of the generated world:
* `semantic_map.json`: provides a high level description of the modules placed according to the spec outlined in [semantic_map_spec.pdf](semantic_map_spec.pdf)
* `low_level_map.json`: provides a low level description of the world such that there is only a list of blocks. This mainly affects AABBs since all the blocks they are composed of are just added to the final list of blocks. All high level information about what the blocks represent is lost.

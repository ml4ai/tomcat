## Library Prerequisites
* CMake 3.10 or above
* Boost 1.69
* nlohmann_json

## Visualizer Prerequisites
* Python 3.7+
* matplotlib (3.3.3 was used in development)
* numpy (1.19 was used in development)
* pygraphviz (1.7 was used in development)

## Installation Steps

Follow the steps below to install `mcg`. Details about the Minecraft
Java code that reads the JSON produced by the library is given later in the
document.

* Clone the [ToMCAT repository](https://github.com/ml4ai/tomcat/tree/master.)
* Do the following to build the library with the entire ToMCAT project. Building ToMCAT is necessary for visualizing the world in Minecraft.
  * To compile and build the library without the tutorial world, do either one of the following:
    * In the `tomcat` directory, run `./tools/install`
    * From the `tomcat` directory,create and move into `build` with `mkdir build && cd build`. Then, run `cmake .. && make -j` 
  * To compile it with the tutorial world, use:
    * In the `tomcat` directory, run `mkdir build & cd build`. Then, run `cmake .. -DBUILD_EXAMPLES=ON && make -j`
* To compile and build just the library:
  * In `libs/mcg/` run `mkdir build && cd build`. Then run `cmake .. && make -j`

## Running the library's tutorial world
* To visualize the library's tutorial world, the tomcat repo contains a script that can be run as follows:
  * If the library was rebuilt with the example after saving all recent changes to the code, simply run `./tools/run_mcg_tutorial` from the `tomcat` directory. 
  * Otherwise, do the following:
    * From the `tomcat` directory, run `mkdir build && cd build` (Only make the build directory if it isn't already present)
    * Then, run `cmake .. -DBUILD_EXAMPLES=ON && make -j && ../tools/rug_mcg_tutorial` to recompile and run the tutorial.

## Minecraft Java Code

In ToMCAT, a class called `WorldBuilder` was created to read and index
everything to place from the library's procedural generator output. The class
can be found
[here](https://github.com/ml4ai/tomcat/blob/master/external/malmo/Minecraft/src/main/java/edu/arizona/tomcat/Utils/WorldBuilder.java).

Using this class is quite simple. You would simply do:

```java
private void buildStructures(World world) {
        WorldBuilder worldBuilder = new WorldBuilder(); // Create a world builder object
        worldBuilder.build("low_level_map.json", world, false, true); // Send it the world and the JSON to build with
}
```

Here, `low_level_map.json` is the low level JSON output from the library, and
`world` is a Minecraft world as represented in Minecraft's code. The false and
true arguments specify whether the WorldBuilder object should save the indexes
it creates. Saving them can be fairly memory intensive.

[An example of its use in ToMCAT can be found here.](https://github.com/ml4ai/tomcat/blob/master/external/malmo/Minecraft/src/main/java/edu/arizona/tomcat/Mission/ProceduralGenMission.java)

## Visualizing the Map

There is a small python script included in the `mcg` library that can
visualize the maps you create with the library. Assuming you've installed the
required dependencies listed at the top of this file:

* Place the semantic JSON file named as `semantic_map.json` in the same
  directory as the `mcgviz` script.
* Run the script with `python3 mcgviz`
* See the plot in `map_plot.pdf` and the graph in `map_graph.pdf`.

There are also various program options available for the visualizer. You can
learn about them by running `python3 mcgviz -h` or `python3
mcgviz --help`.

## JSON Spec

For each algorithm, the library outputs two JSON representations of the
generated world:

* `semantic_map.json`: provides a high level description of the modules placed
  according to the spec outlined in [semantic_map_spec.pdf](docs/semantic_map_spec.pdf)
* `low_level_map.json`: provides a low level description of the world in two lists:
  * locations: Which lists only blocks. Every block needed to represent every structure is present in this list.
  * entities: A list of the entities to place

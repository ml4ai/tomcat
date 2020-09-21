#include "GridWorld.h"
#include "ZombieWorld.h"
#include <boost/program_options.hpp>
#include <fstream>
#include <iostream>

using namespace std;
namespace po = boost::program_options;

/**
 * @brief Directive method to create the world and write the JSON and TSV
 * output to file.
 */
int main(int argc, char* argv[]) {

    int choice, seed;
    string semantic_map_json_path, low_level_map_json_path;

    // Handle options
    po::options_description general("Allowed options");
    general.add_options()("help,h", "Show program options")(
        "world_type",
        po::value<int>(&choice)->default_value(0),
        "Type of world to generate.\nZombieworld = 0\nGridworld = "
        "1")("help_gridworld",
             "Show additional arguments specific to Gridworld generation.")(
        "semantic_map_path",
        po::value<string>(&semantic_map_json_path)->default_value("semantic_map.json"),
        "Path to the output semantic map JSON file.")(
        "low_level_map_path",
        po::value<string>(&low_level_map_json_path)
            ->default_value("low_level_map.json"),
        "Path to the low level (more granular) map JSON file.")(
        "seed",
        po::value<int>(&seed)->default_value(0),
        "The seed used to initialize the random number generator.");

    po::options_description gridWorldOptions(
        "Allowed options for the Gridworld generator");
    gridWorldOptions.add_options()("N",
                                   po::value<int>()->default_value(3),
                                   "Number of AABB per axis of the gridworld.")(
        "sep",
        po::value<int>()->default_value(0),
        "The separation between AABB in the cardinal directions.")(
        "AABB_size",
        po::value<int>()->default_value(10),
        "The size of the cubic AABB used.");

    po::options_description all("Allowed options");
    all.add(general).add(gridWorldOptions);

    po::variables_map vm;
    po::store(po::command_line_parser(argc, argv).options(all).run(), vm);
    po::notify(vm);

    if (vm.count("help")) {
        cout << general << endl;
        return 0;
    }
    if (vm.count("help_gridworld")) {
        cout << gridWorldOptions << endl;
        return 0;
    }

    cout << "Generating world..." << endl;
    if (choice == 0) {
        ZombieWorld world(seed);
        world.writeToFile(semantic_map_json_path, low_level_map_json_path);
    }
    else if (choice == 1) {
        int N = vm["N"].as<int>();
        int sep = vm["sep"].as<int>();
        int AABB_size = vm["AABB_size"].as<int>();
        GridWorld world(N, sep, AABB_size, seed);
        world.writeToFile(semantic_map_json_path, low_level_map_json_path);
    }
    else {
        cout << "Your choice is invalid" << endl;
        return 1;
    }

    cout << "Done. The generated files are in " << semantic_map_json_path << " and "
         << low_level_map_json_path << endl;
    return 0;
}

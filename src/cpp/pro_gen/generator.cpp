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
    string jsonPath, tsvPath;

    // Handle options
    po::options_description general("Allowed options");
    general.add_options()("help,h", "Show program options")(
        "gen_type",
        po::value<int>(&choice)->default_value(0),
        "Choose the type of world to generate.\nZombieworld = 0\nGridworld = "
        "1")("help_gridworld",
             "Show additional arguments specific to Gridworld.")(
        "json_path",
        po::value<string>(&jsonPath)->default_value("procedural.json"),
        "Specify where to save the JSON file with filename and extension.")(
        "tsv_path",
        po::value<string>(&tsvPath)->default_value("proceduralAltJSON.json"),
        "Specify where to save the TSV file with filename and extension.")(
        "seed",
        po::value<int>(&seed)->default_value(0),
        "The seed used to initialize the random "
        "object used within the generator.");

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
        world.writeToFile(jsonPath, tsvPath);
    }
    else if (choice == 1) {
        int N = vm["N"].as<int>();
        int sep = vm["sep"].as<int>();
        int AABB_size = vm["AABB_size"].as<int>();
        GridWorld world(N, sep, AABB_size, seed);
        world.writeToFile(jsonPath, tsvPath);
    }
    else {
        cout << "You choice is invalid" << endl;
        return 1;
    }

    cout << "Done. The generated files are in " << jsonPath << " and "
         << tsvPath << endl;
    return 0;
}
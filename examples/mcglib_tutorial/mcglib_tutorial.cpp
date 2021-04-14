#include "TutorialWorld/TutorialWorld.h"
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

    int world_type, seed;
    string semantic_map_json_path, low_level_map_json_path;

    // Handle options
    po::options_description general("Allowed options");
    general.add_options()("help,h", "Show program options")(
        "semantic_map_path",
        po::value<string>(&semantic_map_json_path)
            ->default_value("semantic_map.json"),
        "Path to the output semantic map JSON file.")(
        "low_level_map_path",
        po::value<string>(&low_level_map_json_path)
            ->default_value("low_level_map.json"),
        "Path to the low level (more granular) map JSON file.");


    po::options_description all("Allowed options");

    po::variables_map vm;
    po::store(po::command_line_parser(argc, argv).options(all).run(), vm);
    po::notify(vm);

    if (vm.count("help")) {
        cout << general << endl;
        return 0;
    }

    cout << "Generating world..." << endl;

    cout << "Done. The generated files are in " << semantic_map_json_path
         << " and " << low_level_map_json_path << endl;
    return 0;
}

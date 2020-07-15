#include "ZombieworldGenerator.h"
using namespace std;
namespace po = boost::program_options;

/**
 * @brief Directive method to create the world and write the JSON and TSV
 * output to file.
 */
int main(int argc, char* argv[]) {

    string jsonPath, tsvPath;

    // Handle options
    po::options_description desc("Allowed options");
    desc.add_options()("help,h", "produce help message")(
        "json_path",
        po::value<string>()->default_value("procedural.json"),
        "Specify where to save the JSON file with filename and extension.")(
        "tsv_path",
        po::value<string>()->default_value("procedural.tsv"),
        "Specify where to save the TSV file with filename an extension.");

    po::variables_map vm;
    po::store(po::command_line_parser(argc, argv).options(desc).run(), vm);
    po::notify(vm);

    if (vm.count("help")) {
        cout << desc << endl;
        return 0;
    }

    if (vm.count("json_path")) {
        jsonPath = vm["json_path"].as<string>();
    }

    if (vm.count("tsv_path")) {
        tsvPath = vm["tsv_path"].as<string>();
    }

    // Process input and generate output
    cout << "Generating zombieworld..." << endl;
    ZombieWorldGenerator zwg(20);
    World&  world = zwg.getWorld();
    cout << "Writing to file..." << endl;

    // Write JSON
    ofstream outputJSON(jsonPath, ios::out);
    outputJSON << world.toJSON();
    outputJSON.close();

    // Write TSV
    ofstream outputTSV(tsvPath, ios::out);
    outputTSV << world.toTSV();
    outputTSV.close();

    cout << "Done. The generated files are in Minecraft/run/procedural.json "
            "and Minecraft/run/procedural.tsv"
         << endl;

    return 0;
}
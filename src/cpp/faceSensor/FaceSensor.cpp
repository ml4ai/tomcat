#include "WebcamSensor.h"
#include <csignal>
#include <iostream>
#include <string>

// Boost includes
#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>
#include <boost/program_options.hpp>

using namespace std;
using namespace tomcat;
namespace po = boost::program_options;

int main(int ac, char* av[]) {
    string exp_id, trial_id, playername, of_dir, file_path;
    bool indent, visualize;
    // Boost command line options
    try {

        po::options_description desc("Allowed options");
        desc.add_options()("help,h", "Show this help message")(
            "exp_id",
            po::value<string>(&exp_id)->default_value(""),
            "Set experiment ID")(
            "trial_id",
            po::value<string>(&trial_id)->default_value(""),
            "set trial ID")("playername",
                            po::value<string>(&playername)->default_value(""),
                            "Set player name")(
            "mloc", po::value<string>(&of_dir), "Set OpenFace models directory")(
            "indent",
            po::bool_switch(&indent)->default_value(false),
            "Indent output JSON by four spaces")(
            "visualize",
            po::bool_switch(&visualize)->default_value(false),
            "Enable visualization")(
            "file,f",
            po::value<string>(&file_path)->default_value("null"),
            "Specify an input video file");

        po::variables_map vm;
        po::store(po::parse_command_line(ac, av, desc), vm);
        po::notify(vm);

        if (vm.count("help")) {
            cout << desc << "\n";
            return 0;
        }

        if (getenv("OPENFACE_MODELS_DIR") != NULL) {
            if (vm.count("mloc")) {
                char* path = &of_dir[0];
                setenv("OPENFACE_MODELS_DIR", path, 1);
            }
        }
        else if (vm.count("mloc")) {
            char* path = &of_dir[0];
            setenv("OPENFACE_MODELS_DIR", path, 0);
        }
        else {
            throw runtime_error(
                "OPENFACE_MODELS_DIR is not set. Use the --mloc flag or "
                "set the environment variable to point to the directory "
                "containing the OpenFace models. Exiting now.");
        }
    }
    catch (exception const& e) {
        cerr << "error: " << e.what() << endl;
        return 1;
    }
    catch (...) {
        cerr << "Exception of unknown type!" << endl;
    }

    WebcamSensor camsensor;
    camsensor.initialize(
        exp_id, trial_id, playername, indent, visualize, file_path);
    camsensor.get_observation();

    return 0;
}

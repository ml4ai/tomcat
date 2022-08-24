#include <chrono>
#include <csignal>
#include <fstream>
#include <functional>
#include <iostream>
#include <queue>

#include <boost/filesystem.hpp>
#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include <boost/program_options.hpp>

#include "Configurator.hpp"
#include "MqttAgent.hpp"
#include "FileAgent.hpp"
#include "Utils.hpp"

// An extendable base class for Testbed Agents
// Authors:   Joseph Astier, Adarsh Pyareral

namespace po = boost::program_options;
namespace fs = boost::filesystem;
namespace json = boost::json;

using namespace std;
using namespace std::chrono;

namespace {
    volatile std::sig_atomic_t gSignalStatus;
}

void signal_handler(int signal) { gSignalStatus = signal; }


int main(int argc, char* argv[]) {

    Utils utils;

    // get configuration
    Configurator configurator;
    json::object config = configurator.parse_args(argc, argv);

    // If the user has specified input and output filenames, run 
    // in file mode
    json::object file = utils.val<json::object>(config, "file");
    string input_file = utils.val<string>(file, "in");
    string output_file = utils.val<string>(file, "out");

    // if neither filename is specified, run in MQTT mode
    if(input_file.empty() && output_file.empty()) {

        MqttAgent mqtt_agent(config);
        mqtt_agent.start();

        signal(SIGINT, signal_handler);

        while (true) {
            if (gSignalStatus == SIGINT) {
                BOOST_LOG_TRIVIAL(info)
                    << "Keyboard interrupt detected (Ctrl-C), shutting down.";

                mqtt_agent.stop();
                break;
            }
            else {
                this_thread::sleep_for(milliseconds(100));
            }
        }
    }
    else {
	// otherwise run in file mode
        FileAgent file_agent(config);
    }

    return EXIT_SUCCESS;
}

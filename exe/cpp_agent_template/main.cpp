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

    // get configuration
    Configurator configurator;
    json::object config = configurator.parse_args(argc, argv);

    // If the user has specified input and output filenames, run 
    // in file mode
    json::object file = json::value_to<json::object>(config.at("file"));
    string input_file = json::value_to<string>(file.at("in"));
    string output_file = json::value_to<string>(file.at("out"));

    // if either or both of the filenames are specified, run in File mode
    if(!input_file.empty() || !output_file.empty()) {
        FileAgent file_agent(config);
    }

    else {
        // Otherwise run in MQTT mode
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
    return EXIT_SUCCESS;
}

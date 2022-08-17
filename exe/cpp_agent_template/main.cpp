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

    cout << "Starting C++ Template Agent..." << endl;
    
    // get configuration
    Configurator configurator;
    json::object config = configurator.parse_args(argc, argv);

    string version = json::value_to<string>(config.at("version"));

    // run this or the file agent based on config input
    MqttAgent mqtt_agent(config);
    mqtt_agent.start();

    cout << "C++ Template Agent version " << version << " running." << endl;

    signal(SIGINT, signal_handler);

    while (true) {
        if (gSignalStatus == SIGINT) {
            BOOST_LOG_TRIVIAL(info)
                << "Keyboard interrupt detected (Ctrl-C), shutting down.";
            coordinator.stop();
            break;
        }
        else {
            this_thread::sleep_for(milliseconds(100));
        }
    }

    return EXIT_SUCCESS;
}

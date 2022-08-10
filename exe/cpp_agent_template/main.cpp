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
#include "Coordinator.hpp"

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

    cout << "Configuration:" << endl;
    cout << config << endl;

    Coordinator coordinator(config);

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

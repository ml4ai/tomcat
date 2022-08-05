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

#include "ReferenceAgent.hpp"
#include "Config.hpp"

// An extendable base class for Testbed Agents
// Authors:   Joseph Astier, Adarsh Pyareral

namespace po = boost::program_options;
namespace fs = boost::filesystem;

using namespace std;
using namespace std::chrono;
//using namespace boost::json;

namespace {
    volatile std::sig_atomic_t gSignalStatus;
}

void signal_handler(int signal) { gSignalStatus = signal; }


int main(int argc, char* argv[]) {

    // get configuration
    Config config;
    string configuration = config.parse_args(argc, argv);

    cout << "Configuration:" << endl;
    cout << configuration << endl;

    // glean these from JSON configuration
    string host = "localhost";
    int port = 1883;
    string input_topic = "input";
    string output_topic = "output";


    ReferenceAgent agent(host, port, input_topic, output_topic);

    signal(SIGINT, signal_handler);

    while (true) {
        if (gSignalStatus == SIGINT) {
            BOOST_LOG_TRIVIAL(info)
                << "Keyboard interrupt detected (Ctrl-C), shutting down.";
            agent.stop();
            break;
        }
        else {
            this_thread::sleep_for(milliseconds(100));
        }
    }

    return EXIT_SUCCESS;
}

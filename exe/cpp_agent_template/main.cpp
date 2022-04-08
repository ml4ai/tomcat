#include <chrono>
#include <csignal>
#include <fstream>
#include <functional>
#include <iostream>
#include <queue>

#include <boost/filesystem.hpp>
#include <boost/program_options.hpp>

#include "ReferenceAgent.hpp"

namespace po = boost::program_options;
namespace json = boost::json;
namespace fs = boost::filesystem;

using namespace std;
using namespace std::chrono;

namespace {
    volatile std::sig_atomic_t gSignalStatus;
}

void signal_handler(int signal) { gSignalStatus = signal; }


int main(int argc, char* argv[]) {

    // Setting up program options
    po::options_description generic("Generic options");

    string config_path;
    generic.add_options()("help,h", "Display this help message")(
        "version,v",
        "Display the version number")("config,c",
                                      po::value<string>(&config_path),
                                      "Path to (optional) config file.");

    po::options_description config("Configuration");

    config.add_options()("mqtt.host",
                         po::value<string>()->default_value("localhost"),
                         "MQTT broker host")(
        "mqtt.port", po::value<int>()->default_value(1883), "MQTT broker port");

    po::options_description cmdline_options;
    cmdline_options.add(generic).add(config);

    po::variables_map vm;
    po::store(po::parse_command_line(argc, argv, cmdline_options), vm);

    // We run notify this first time to pick up the -c/--config option
    po::notify(vm);

    // Print a help message
    if (vm.count("help")) {
        cout << cmdline_options;
        return 1;
    }

    // If the -c/--config option is passed to the program on the command line,
    // we check for the existence of the specified config file and load the
    // options from it.
    if (vm.count("config")) {
        if (fs::exists(config_path)) {
            po::store(po::parse_config_file(config_path.c_str(), config), vm);
        }
        else {
            BOOST_LOG_TRIVIAL(error) << "Specified config file '" << config_path
                                     << "' does not exist!";
            return EXIT_FAILURE;
        }
    }

    // We run the notify function a second time in order to process the config
    // file
    po::notify(vm);

    string address = "tcp://" + vm["mqtt.host"].as<string>() + ":" +
                     to_string(vm["mqtt.port"].as<int>());

    signal(SIGINT, signal_handler);

    ReferenceAgent agent(address);
    while (true) {
        if (gSignalStatus == SIGINT) {
            BOOST_LOG_TRIVIAL(info)
                << "Keyboard interrupt detected (Ctrl-C), shutting down.";
            break;
        }
        else {
            this_thread::sleep_for(milliseconds(100));
        }
    }

    return EXIT_SUCCESS;
}

#include "Config.hpp"

#include <boost/filesystem.hpp>
#include <boost/iostreams/device/file.hpp>
#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include <boost/program_options.hpp>
#include <stdlib.h>
#include <iostream>

namespace po = boost::program_options;
namespace fs = boost::filesystem;

using namespace std;

string Config::parse_args(int argc, char* argv[]) {

    // set up options
    po::options_description options = describe_options();

    // parse command line args into options
    po::variables_map vm;
    po::store(po::parse_command_line(argc, argv, options), vm);
    po::notify(vm);

    // if the user wants the help page, show it and exit
    if (vm.count("help")) {
        cout << options << endl;
        exit(EXIT_SUCCESS);
    }

    // parse the config file
    string config_filename = vm["config"].as<string>();
    string config_text =read_text_file(config_filename);

    // if the user wants the software version, show it and exit
    if (vm.count("version")) {
        cout << "version_from_config_file" << endl;
        exit(EXIT_SUCCESS);
    }

    // Compose JSON with completed configuration
    // ...

    return config_text;
}


// return an options description of all command line inputs
po::options_description Config::describe_options(){

    po::options_description options("Configuration");
    options.add_options()
        ("help,h", "Display this help message")
        ("version,v", "Display the version number")
        ("config,c", 
            po::value<string>()->default_value("../config.json"),
            "Specify a config file"
        )
        ("mqtt.host",
            po::value<string>()->default_value("localhost"),
            "MQTT broker host"
        )
        ("mqtt.port",
            po::value<int>()->default_value(1883), 
            "MQTT broker port"
        )   
    ;

    return options;
}


// return the contents of the file as a single plaintext string
string Config::read_text_file(string filename){


    // Read the config file as plaintext
    ifstream ifs(filename);
    if(!ifs) {
        BOOST_LOG_TRIVIAL(error) 
            << "Could not read logfile '" 
            << filename
        ;
        exit(EXIT_FAILURE);
    }

    // concatenate all lines in the text file
    std::string text;
    for (std::string line; std::getline(ifs, line); ) {
	text += line;
    }
    return text;
}

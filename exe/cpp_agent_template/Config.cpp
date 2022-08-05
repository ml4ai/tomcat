#include "Configurator.hpp"

#include <boost/filesystem.hpp>
#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include <boost/program_options.hpp>
#include <stdlib.h>
#include <iostream>

namespace po = boost::program_options;
namespace fs = boost::filesystem;

using namespace std;

string Configurator::parse_args(int argc, char* argv[]) {

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
    string config_file_json = parse_config_file(config_filename);

    // if the user wants the software version, show it and exit
    if (vm.count("version")) {
        cout << "version_from_config_file" << endl;
        exit(EXIT_SUCCESS);
    }

    // Compose JSON with completed configuration
    // ...

    return config_file_json;
}


// return an options description of all command line inputs
po::options_description Configurator::describe_options(){

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


// read the JSON config file
// TODO return JSON object
string Configurator::parse_config_file(string filename){

    // Read the config file as plaintext
    if (fs::exists(filename)) {

        // read the JSON and return it
        //  ...

	// TODO replace this dummy value with the file contents
	string file_json = 
	    "{\"input_topic\": \"foo\", \"output_topic\": \"bar\"}";
	return file_json;
    }
    else {
        BOOST_LOG_TRIVIAL(error) 
            << "Specified config file '" 
            << filename
            << "' does not exist!"
        ;
        exit(EXIT_FAILURE);
    }
}

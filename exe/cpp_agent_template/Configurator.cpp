#include "Configurator.hpp"

#include <boost/filesystem.hpp>
#include <boost/iostreams/device/file.hpp>
#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include <boost/program_options.hpp>
#include <stdlib.h>
#include <iostream>

namespace po = boost::program_options;
namespace fs = boost::filesystem;
namespace json = boost::json;

using namespace std;

json::object Configurator::parse_args(int argc, char* argv[]) {

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

    // parse the config file as JSON
    string config_filename = vm["config"].as<string>();
    json::object config = parse_config_file(config_filename);

    // Compose JSON with completed configuration
    // ...

    // Validate configuration
    validate(config);

    // if the user wants the software version, show it and exit
    if (vm.count("version")) {
        cout << "version_from_config_file" << endl;
        exit(EXIT_SUCCESS);
    }

    return config;
}

// Test that required fields are present.  
void Configurator::validate(json::object config){
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


// return the contents of the config file as a JSON object
json::object Configurator:: parse_config_file(string filename){

    // Read the config file as plaintext
    ifstream ifs(filename);
    if(!ifs) {
        BOOST_LOG_TRIVIAL(error) << "Could not read file: " << filename;
        exit(EXIT_FAILURE);
    }

    json::stream_parser p;
    error_code ec;

    // compose JSON from all lines in config file.  The file can
    // be in pretty or compact JSON format.
    for (string line; std::getline(ifs, line); ) {
        p.write_some(line, ec);
	if(ec) {
            BOOST_LOG_TRIVIAL(error) << "JSON error parsing: " << line;
            exit(EXIT_FAILURE);
        }
    }
    ifs.close();

    assert(p.done());
    return p.release().as_object();
}

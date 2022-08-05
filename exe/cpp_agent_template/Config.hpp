#pragma once

#include <boost/filesystem.hpp>
#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include <boost/program_options.hpp>

// Authors:   Joseph Astier, Adarsh Pyareral

namespace po = boost::program_options;
using namespace std;


// This class handles configuring the Agents from file and user input
class Configurator {

    public:
	// return a JSON object based on command line arguments
        string parse_args(int argc, char* argv[]);

    private:
	// describe options
	po::options_description describe_options();

        // parse config file
        string parse_config_file(string filename);
};

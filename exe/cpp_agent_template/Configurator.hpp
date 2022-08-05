#pragma once

#include <boost/filesystem.hpp>
#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include <boost/program_options.hpp>

// Authors:   Joseph Astier, Adarsh Pyareral

namespace po = boost::program_options;
namespace json = boost::json;

using namespace std;


// This class handles configuring the Agents from file and user input
class Configurator {

    public:
        // return a JSON object based on command line arguments
        json::object parse_args(int argc, char* argv[]);

    private:
        // describe options
        po::options_description describe_options();

        // return the contents of a file as plaintext
        json::object parse_config_file(string filename);

	// Check that required configuration fields are present
	void validate(json::object config);
};

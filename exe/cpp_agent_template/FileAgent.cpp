#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include <iostream>
#include <fstream>
//#include <string>
#include "FileAgent.hpp"

using namespace std;
namespace json = boost::json;

FileAgent::FileAgent(const json::object &config) {

    Agent::configure(config);

    cout << "Running in File Mode" << endl;

    // Input and output filenames must both be specified
    // otherwise report a a configuration error
    json::object file_config = json::value_to<json::object>(config.at("file"));
    string input_filename = json::value_to<string>(file_config.at("in"));
    string output_filename = json::value_to<string>(file_config.at("out"));
    if(input_filename.empty() || output_filename.empty()) {
        cerr << "file.in and file.out must be specified in file mode" << endl;
        exit(EXIT_FAILURE);
    }

    // open input file for reading
    ifstream input_file;
    input_file.open(input_filename);
    if(input_file.is_open()) {
        cout << "Input file: " << input_filename << endl;
    } else {
	cerr << "Could not open " << input_filename << " for reading" << endl;
        exit(EXIT_FAILURE);
    }

    // open output file for writing
    output_file.open(output_filename);
    if(output_file.is_open()) {
        cout << "Output file: " << output_filename << endl;
    } else {
	cerr << "Could not open " << output_filename << " for writing" << endl;
        exit(EXIT_FAILURE);
    } 

    // process the input file 
    cout << "Processing..." << endl;
    string line;
    while(std::getline(input_file, line)) {
        json::object message = parse_json(line);
        message_handler.process_message(message);
    }

    // shutdown
    input_file.close();
    output_file.close();
    cout << "File processing complete." << endl;
}

// write to filesystem, include the topic in the message
void FileAgent::publish(json::object &message) {
    output_file << json::serialize(message) << endl;
}

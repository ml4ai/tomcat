#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include <iostream>
#include <fstream>
//#include <string>
#include "FileAgent.hpp"

using namespace std;
namespace json = boost::json;

FileAgent::FileAgent(const json::object &config) : Agent(config) {
    cout << "Running in File Mode" << endl;

    json::object file_config = json::value_to<json::object>(config.at("file"));
    input_filename = json::value_to<string>(file_config.at("in"));
    output_filename = json::value_to<string>(file_config.at("out"));

    // both files must be specified
    // otherwise report a a configuration error
    if(input_filename.empty() || output_filename.empty()) {
        cerr << "file.in and file.out must be specified in file mode" << endl;
        exit(EXIT_FAILURE);
    }

    // open input file for reading
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

    process_file();
}

void FileAgent::process_file() {
    cout << "Processing..." << endl;
    string line;

    while(std::getline(input_file, line)) {
        process_line(line);
    }

    input_file.close();
    output_file.close();
    cout << "Done." << endl;
}

void FileAgent::process_line(const string line) {
    json::object message = parse_json(line);
    process_message(message);
}

// write to filesystem, include the topic in the message
void FileAgent::publish(json::object &message) {
    output_file << json::serialize(message) << endl;
}

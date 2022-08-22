#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include <iostream>
#include "FileAgent.hpp"

using namespace std;
namespace json = boost::json;

FileAgent::FileAgent(const json::object &config) : Agent(config) {
    cout << "File Agent version " << version << endl;

    json::object file = json::value_to<json::object>(config.at("file"));
    string input_file = json::value_to<string>(file.at("in"));
    string output_file = json::value_to<string>(file.at("out"));

    // both files must be specified
    // otherwise report a a configuration error
    if(input_file.empty() || output_file.empty()) {
        cerr << "file.in and file.out must be specified to run in file mode" << endl;
        exit(EXIT_FAILURE);
    }

    cout << "Input file: " << input_file << endl;
    cout << "Output file: " << output_file << endl;
    process_files(input_file, output_file);
}

void FileAgent::process_files(const string input_file,
                              const string output_file) {

    cout << "Processing..." << endl;

    // main loop

    cout << "Done." << endl;

}



// write to filesystem
void FileAgent::write(const string topic, json::object &message) {

    message["topic"] = topic;

    cout << "FileAgent::Write:  " << message << endl;
}

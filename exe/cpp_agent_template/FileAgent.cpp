#include "FileAgent.hpp"
#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include <fstream>
#include <iostream>

namespace json = boost::json;

FileAgent::FileAgent(const json::object& config) {

    Agent::configure(config);

    std::cout << "Running in File Mode" << std::endl;

    // Input and output filenames must both be specified
    // otherwise report a a configuration error
    json::object file_config = val<json::object>(config, "file");
    std::string input_filename = val<std::string>(file_config, "in");
    std::string output_filename = val<std::string>(file_config, "out");
    if (input_filename.empty() || output_filename.empty()) {
        std::cerr << "file.in and file.out must be specified in file mode";
        std::cerr << std::endl;
        exit(EXIT_FAILURE);
    }

    // open input file for reading
    std::ifstream input_file;
    input_file.open(input_filename);
    if (input_file.is_open()) {
        std::cout << "Input file: " << input_filename << std::endl;
    }
    else {
        std::cerr << "Could not open " << input_filename;
        std::cerr << " for reading" << std::endl;
        exit(EXIT_FAILURE);
    }

    // open output file for writing
    output_file.open(output_filename);
    if (output_file.is_open()) {
        std::cout << "Output file: " << output_filename << std::endl;
    }
    else {
        std::cerr << "Could not open " << output_filename;
        std::cerr << " for writing" << std::endl;
        exit(EXIT_FAILURE);
    }

    // process the input file
    std::cout << "Processing input file..." << std::endl;
    std::string line;
    int n = 0;
    while (std::getline(input_file, line)) {
        json::object message = parse_json(line);
        reference_processor.process_message(message);
        n++;
    }

    // shutdown
    input_file.close();
    output_file.close();
    std::cout << "Lines processed: " << n << std::endl;
    std::cout << "File processing complete." << std::endl;
}

// write to filesystem, include the topic in the message
void FileAgent::publish(json::object& message) {
    output_file << json::serialize(message) << std::endl;
}

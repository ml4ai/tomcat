#include "FileAgent.hpp"
#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include <fstream>
#include <iostream>

namespace json = boost::json;

FileAgent::FileAgent(
    const json::object& config,
    Processor &processor) : Agent(processor) {

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
        std::cout << "Input file:\t" << input_filename << std::endl;
    }
    else {
        std::cerr << "Could not open " << input_filename;
        std::cerr << " for reading" << std::endl;
        exit(EXIT_FAILURE);
    }

    // open output file for writing
    output_file.open(output_filename);
    if (output_file.is_open()) {
        std::cout << "Output file:\t" << output_filename << std::endl;
    }
    else {
        input_file.close();
        std::cerr << "Could not open " << output_filename;
        std::cerr << " for writing" << std::endl;
        exit(EXIT_FAILURE);
    }

    // process the input file
    std::cout << "Processing input file..." << std::endl;
    std::string line;
    int line_count = 0;
    while (std::getline(input_file, line)) {
        json::object message = parse_json(line);
	process_message(message);
        line_count ++;
    }

    // shutdown file operations
    input_file.close();
    output_file.close();
    std::cout << "Input file lines processed: " << line_count << std::endl;
    std::cout << "File processing complete." << std::endl;
}

// write the message to the output file, including the topic
void FileAgent::publish(json::object& message) {
    output_file << json::serialize(message) << std::endl;
}

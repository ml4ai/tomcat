#pragma once

#include <boost/json.hpp>
#include "Agent.hpp"
#include <fstream>


namespace json = boost::json;

using namespace std;


/** Agent class that manages MQTT traffic  */
class FileAgent : public Agent {

    void process_line(const string line);

    ofstream output_file = ofstream();
    ifstream input_file = ifstream();

    json::stream_parser json_parser;

    string input_filename, output_filename;

    void process_file();

    public:


    // Constructor
    FileAgent(const json::object &config);

    // write output to the filesystem 
    void publish(const string topic, json::object &message) override;
};

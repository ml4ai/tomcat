#pragma once

#include <boost/json.hpp>
#include "Agent.hpp"

namespace json = boost::json;

using namespace std;


/** Agent class that manages MQTT traffic  */
class FileAgent : public Agent {

    public:

    void process_files(const string input_file, const string output_file);

    // Constructor
    FileAgent(const json::object &config);

    // write output to the filesystem 
    void write(const string topic, json::object &message) override;
};

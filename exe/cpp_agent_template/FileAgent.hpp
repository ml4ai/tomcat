#pragma once

#include <boost/json.hpp>
#include "Agent.hpp"
#include <fstream>


namespace json = boost::json;

using namespace std;


/** Agent class that manages MQTT traffic  */
class FileAgent : public Agent {

    ofstream output_file;

    public:

    // Constructor
    FileAgent(const json::object &config);

    // write output to the filesystem 
    void publish(json::object &message) override;
};

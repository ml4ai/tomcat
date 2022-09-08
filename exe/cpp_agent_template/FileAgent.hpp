#pragma once

#include "Agent.hpp"
#include <boost/json.hpp>
#include <fstream>

namespace json = boost::json;

/** Agent class that manages MQTT traffic  */
class FileAgent : public Agent {

    std::ofstream output_file;

  public:
    // Constructor
    FileAgent(const json::object& config, Processor &processor);

    // write output to the filesystem
    void publish(const json::object& message) override;
};

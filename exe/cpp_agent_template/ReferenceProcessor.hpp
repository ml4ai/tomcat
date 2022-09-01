#pragma once

#include "Processor.hpp"
#include <boost/json.hpp>

namespace json = boost::json;

class Agent;

class ReferenceProcessor : public Processor {

    // topics found in the config file
    std::vector<std::string> config_input_topics;
    std::vector<std::string> config_output_topics;

  public:
    void configure(const json::object& config) override;

    ReferenceProcessor(Agent* agent) : Processor(agent) {}

    void process_message(const json::object& message) override;
};

#pragma once

#include "Processor.hpp"
#include <boost/json.hpp>

namespace json = boost::json;

class Agent;

class ReferenceProcessor : public Processor {

    // topics found in the config file
    std::vector<std::string> input_topics, output_topics;

  public:
    ReferenceProcessor(Agent* agent) : Processor(agent) {}

    void configure(const json::object& config) override;

    void process_message(const json::object& message) override;
};

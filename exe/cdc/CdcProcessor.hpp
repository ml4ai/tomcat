#pragma once

#include "Processor.hpp"
#include <boost/json.hpp>

namespace json = boost::json;

class Agent;

class CdcProcessor : public Processor {

    // topics found in the config file
    std::vector<std::string> input_topics, output_topics;

  public:

    CdcProcessor();

    void configure(const json::object& config, Agent *agent);

    void process_message(const json::object& message) override;
};

#pragma once

#include <boost/json.hpp>
#include "Processor.hpp"

namespace json = boost::json;

class Agent;

class ReferenceProcessor: public Processor {

    // all subscribed topics
    std::vector<std::string> input_topics;

    // all publication topics
    std::vector<std::string> output_topics;

    public:

    void configure(const json::object &config) override;
    
    ReferenceProcessor(Agent* agent) : Processor(agent) {}

    void process_message(const json::object &message) override;
};

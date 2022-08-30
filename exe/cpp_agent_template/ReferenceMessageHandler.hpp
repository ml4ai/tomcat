#pragma once

#include <boost/json.hpp>
#include "BaseMessageHandler.hpp"

namespace json = boost::json;

class Agent;

class ReferenceMessageHandler: public BaseMessageHandler {

    // all subscribed topics
    std::vector<std::string> input_topics;

    // all publication topics
    std::vector<std::string> output_topics;

    public:

    void configure(const json::object &config) override;
    
    ReferenceMessageHandler(Agent* agent) : BaseMessageHandler(agent) {}

    void process_message(const json::object &message) override;
};

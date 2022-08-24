#pragma once

#include <string>
#include <boost/json.hpp>
#include "BaseMessageHandler.hpp"

namespace json = boost::json;
using namespace std;

class Agent;

class ReferenceMessageHandler: public BaseMessageHandler {

    // all subscribed topics
    vector<string> input_topics;

    // all publication topics
    vector<string> output_topics;

    public:

    void configure(const json::object &config) override;
    
    ReferenceMessageHandler(Agent* agent) : BaseMessageHandler(agent) {}

    void process_message(const string topic,
                         const json::object &message) override;
};

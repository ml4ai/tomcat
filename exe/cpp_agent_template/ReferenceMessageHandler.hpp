#pragma once

#include <string>
#include <boost/json.hpp>
#include "BaseMessageHandler.hpp"

namespace json = boost::json;
using namespace std;

class Agent;

class ReferenceMessageHandler: public BaseMessageHandler {

    vector<string> input_topics, output_topics;

    public:

    ReferenceMessageHandler(Agent* agent, const json::object &config);
    ReferenceMessageHandler(){}
    ~ReferenceMessageHandler(){}

    void process_message(const string topic,
                         const json::object &message) override;
};

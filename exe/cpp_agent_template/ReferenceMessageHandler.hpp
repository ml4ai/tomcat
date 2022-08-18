#pragma once

#include <string>
#include <boost/json.hpp>
#include "MessageHandler.hpp"

namespace json = boost::json;
using namespace std;

class Agent;

class ReferenceMessageHandler: public MessageHandler {

    string get_input_config_name() override {return "reference_agent_input";}

    string get_output_config_name() override {return "reference_agent_output";}

    json::object get_data(json::object input_data) override;

    void configure(json::object config, Agent *agent) override;

    json::object data = json::object();

    int counter = 0; // dummy state data
};

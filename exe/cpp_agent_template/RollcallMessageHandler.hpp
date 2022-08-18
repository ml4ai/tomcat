#pragma once

#include <string>
#include <boost/json.hpp>
#include "MessageHandler.hpp"

namespace json = boost::json;
using namespace std;

class Agent;

class RollcallMessageHandler: public MessageHandler {

    string get_input_config_name() override { return "rollcall_request";}

    string get_output_config_name() override { return "rollcall_response";}

    json::object get_data(json::object input_data) override;

    void configure(json::object config, Agent *agent) override;

    json::object data;
};

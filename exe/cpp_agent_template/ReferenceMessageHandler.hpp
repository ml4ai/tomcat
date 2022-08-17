#pragma once

#include <string>
#include <boost/json.hpp>
#include "MessageHandler.hpp"

namespace json = boost::json;
using namespace std;


class ReferenceMessageHandler: public MessageHandler {

    string get_input_config_name() override {return "reference_agent_input";}

    string get_output_config_name() override {return "reference_agent_output";}

    json::object get_data(json::object input_data) override;

    void configure(
        json::object config,
        std::shared_ptr<mqtt::async_client> mqtt_client
    ) override;

    json::object data;
};

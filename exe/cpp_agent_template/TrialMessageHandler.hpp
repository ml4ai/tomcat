#pragma once

#include <string>
#include <boost/json.hpp>
#include "MessageHandler.hpp"

namespace json = boost::json;
using namespace std;


class TrialMessageHandler: public MessageHandler {

    string get_input_config_name() override { return "trial_start";}

    string get_output_config_name() override { return "version_info";}

    json::object create_output_data(json::object config) override;

    void configure(
        json::object config,
        std::shared_ptr<mqtt::async_client> mqtt_client
    ) override ;

    json::object output_data;
};

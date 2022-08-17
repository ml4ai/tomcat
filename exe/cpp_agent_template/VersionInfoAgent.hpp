#pragma once

#include <string>
#include <boost/json.hpp>
#include "BaseAgent.hpp"

namespace json = boost::json;
using namespace std;


class VersionInfoAgent: public BaseAgent {

    json::object get_input_config(json::object config) override;

    json::object get_output_config(json::object config) override;

    json::object create_output_data(json::object config) override;

    json::object output_data = json::object();

    void configure(
        json::object config,
        std::shared_ptr<mqtt::async_client> mqtt_client
    ) override ;

};

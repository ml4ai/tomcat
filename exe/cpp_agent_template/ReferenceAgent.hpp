#pragma once

#include <string>
#include <boost/json.hpp>
#include "BaseAgent.hpp"

namespace json = boost::json;
using namespace std;


class ReferenceAgent: public BaseAgent {

    string get_input_config_name() override {return "reference_agent_input";}

    string get_output_config_name() override {return "reference_agent_output";}

    json::object create_output_data(json::object config) override;
};

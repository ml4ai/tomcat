#pragma once

#include <string>
#include <boost/json.hpp>
#include "BaseAgent.hpp"

namespace json = boost::json;
using namespace std;


class RollcallAgent: public BaseAgent {

    json::object get_input_config(json::object config) override;

    json::object get_output_config(json::object config) override;

    json::object create_output_data(json::object config) override;
};

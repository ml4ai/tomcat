#pragma once

#include <string>
#include <boost/json.hpp>
#include "Processor.hpp"

namespace json = boost::json;
using namespace std;


class ReferenceAgentProcessor: public Processor {

    string get_subscription_name() override {return "reference_agent_input";}
    string get_publication_name() override {return "reference_agent_output";}

    void process_input_message(
        json::object sub_header,
        json::object sub_msg,
        json::object sub_data
    ) override;
};

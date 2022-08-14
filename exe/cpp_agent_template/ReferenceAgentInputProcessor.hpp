#pragma once

#include <string>
#include <boost/json.hpp>
#include "Processor.hpp"

namespace json = boost::json;
using namespace std;


/** user-specified message */
class ReferenceAgentInputProcessor: public Processor {

    public:
    string get_subscription_name(){ return "reference_agent_input";}
    string get_publication_name(){ return "reference_agent_output";}

    void process_input_message(
        json::object input_header,
        json::object input_msg,
        json::object input_data
    ) override;
};

#pragma once

#include <string>
#include <boost/json.hpp>
#include "Processor.hpp"

namespace json = boost::json;
using namespace std;


/** user-specified message */
class ReferenceAgentInputProcessor: public Processor {

    public:
    string get_name(){ return "reference_agent_input_message";}
    
    void process(json::object read_from_bus);
};

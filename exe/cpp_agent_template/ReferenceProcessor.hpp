#pragma once

#include <string>
#include <boost/json.hpp>
#include "Processor.hpp"

namespace json = boost::json;
using namespace std;


/** user-specified message */
class ReferenceProcessor: public Processor {

    public:
    string get_name(){ return "reference_agent_input";}
    
};

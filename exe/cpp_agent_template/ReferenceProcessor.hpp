#pragma once

#include <string>
#include <boost/json.hpp>
#include "Processor.hpp"

namespace json = boost::json;
using namespace std;


/** user-specified message */
class ReferenceProcessor: public Processor {

    public:

    /** Constructor */
    ReferenceProcessor(json::object config);
    
    /** handle message */
    void process(string topic, json::object message);
};

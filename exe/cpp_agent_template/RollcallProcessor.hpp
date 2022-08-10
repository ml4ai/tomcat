#pragma once

#include <string>
#include <boost/json.hpp>
#include "Processor.hpp"


namespace json = boost::json;

using namespace std;


/** trial message */
class RollcallProcessor: public Processor {

    public:

    /** Constructor */
    RollcallProcessor(json::object config);
    
    /** handle message */
    void handle_message(string topic, json::object message);
};

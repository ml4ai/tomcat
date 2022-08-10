#pragma once

#include <string>
#include <boost/json.hpp>
#include "Processor.hpp"

namespace json = boost::json;
using namespace std;


class RollcallProcessor: public Processor {

    public:

    /** Constructor */
    RollcallProcessor(json::object config);
    
    /** handle message */
    void process(string topic, json::object message);
};

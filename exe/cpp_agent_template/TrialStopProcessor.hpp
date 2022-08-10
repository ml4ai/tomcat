#pragma once

#include <string>
#include <boost/json.hpp>
#include "Processor.hpp"


namespace json = boost::json;

using namespace std;


/** trial message */
class TrialStopProcessor: public Processor {

    public:

    /** Constructor */
    TrialStopProcessor(json::object config);

    /** handler call */
    void handle_message(string topic, json::object message);
};

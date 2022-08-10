#pragma once

#include <string>
#include <boost/json.hpp>
#include "Processor.hpp"


namespace json = boost::json;

using namespace std;


/** trial message */
class TrialStartProcessor: public Processor {

    public:

    /** Constructor */
    TrialStartProcessor(json::object config);

    /** handler call */
    void handle_message(string topic, json::object message);
};

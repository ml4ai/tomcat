#pragma once

#include <string>
#include <boost/json.hpp>
#include "Processor.hpp"

namespace json = boost::json;
using namespace std;


/** trial stop message */
class TrialStopProcessor: public Processor {

    public:

    /** Constructor */
    TrialStopProcessor(json::object config);

    /** handle message */
    void process(string topic, json::object message);
};

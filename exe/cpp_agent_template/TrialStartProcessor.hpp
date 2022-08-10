#pragma once

#include <string>
#include <boost/json.hpp>
#include "Processor.hpp"

namespace json = boost::json;
using namespace std;


class TrialStartProcessor: public Processor {

    public:

    /** Constructor */
    TrialStartProcessor(json::object config);

    /** handle message */
    void process(string topic, json::object message);
};

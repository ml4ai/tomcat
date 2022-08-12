#pragma once

#include <string>
#include <boost/json.hpp>
#include "Processor.hpp"

namespace json = boost::json;
using namespace std;


/** trial stop message */
class TrialStopProcessor: public Processor {

    public:

    string get_name() {return "trial_stop";}

    void process(json::object read_from_bus);
};

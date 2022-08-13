#pragma once

#include <string>
#include <boost/json.hpp>
#include "Processor.hpp"

namespace json = boost::json;
using namespace std;


/** trial stop message */
class TrialStopProcessor: public Processor {

    public:

    string get_subscription_name(){ return "trial_stop";}
    string get_publication_name(){ return "";}  // no publication

    void process(
        json::object common_header,
        json::object common_message,
        json::object data
    ) override;
};

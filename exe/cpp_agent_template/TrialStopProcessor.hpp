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

    void process_input_message(
        json::object input_header,
        json::object input_msg,
        json::object input_data
    ) override;
};

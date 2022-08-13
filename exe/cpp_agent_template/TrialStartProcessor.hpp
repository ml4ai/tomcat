#pragma once

#include <string>
#include <boost/json.hpp>
#include "Processor.hpp"

namespace json = boost::json;
using namespace std;


class TrialStartProcessor: public Processor {

    public:
    string get_subscription_name(){ return "trial_start";}
    string get_publication_name(){ return "version_info";}

    void process(
        json::object common_header,
        json::object common_message,
        json::object data
    ) override;
};

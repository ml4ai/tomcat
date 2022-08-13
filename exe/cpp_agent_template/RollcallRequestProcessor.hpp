#pragma once

#include <string>
#include <boost/json.hpp>
#include "Processor.hpp"

namespace json = boost::json;
using namespace std;


class RollcallRequestProcessor: public Processor {

    string get_subscription_name(){ return "rollcall_request";}
    string get_publication_name(){ return "rollcall_response";}

    void process(
        json::object common_header,
        json::object common_message,
        json::object data
    ) override;
};

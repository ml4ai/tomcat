#pragma once

#include <string>
#include <boost/json.hpp>
#include "Processor.hpp"

namespace json = boost::json;
using namespace std;


class RollcallRequestProcessor: public Processor {

    // input
    json::object rollcall_request_config;
    string rollcall_request_topic;

    // output
    json::object rollcall_response_config;
    string rollcall_response_topic;
    string rollcall_response_message_type;
    string rollcall_response_sub_type;

    void process_input_message(string topic,json::object message) override;
    void configure(json::object config) override;
};

#pragma once

#include <string>
#include <boost/json.hpp>
#include "Processor.hpp"

namespace json = boost::json;
using namespace std;


class RollcallRequestProcessor: public Processor {

    // input
    json::object input_config;
    string input_topic;

    // output
    json::object output_config;
    string output_topic;
    string output_message_type;
    string output_sub_type;

    void process_input_message(string topic,json::object message) override;
    void configure(json::object config) override;
};

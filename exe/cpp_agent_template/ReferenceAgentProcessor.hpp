#pragma once

#include <string>
#include <boost/json.hpp>
#include "Processor.hpp"

namespace json = boost::json;
using namespace std;


class ReferenceAgentProcessor: public Processor {

    // input
    json::object input_config;
    string input_topic;

    // output
    json::object output_config;
    string output_topic;

    void process_input_message(string topic,json::object message) override;
    void configure(json::object config) override;
};

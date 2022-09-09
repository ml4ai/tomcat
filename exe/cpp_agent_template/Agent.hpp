#pragma once

#include "Processor.hpp"
#include "Utils.hpp"
#include <boost/json.hpp>
#include <iostream>

namespace json = boost::json;

// interface for write method
class Agent : public Utils {

    // only process JSON messages
    json::stream_parser json_parser;

    // messages processed
    std::vector<std::string> subscription_activity;

    // messages published
    std::vector<std::string> publication_activity;

    protected:

    void log_subscription_activity(const std::string topic);
    void log_publication_activity(const std::string topic);
    void summarize_activity();

    Processor &processor; 

    std::string app_name;

    json::object parse_json(const std::string text);

    void configure(const json::object& config);

    void process_message(const json::object& input_message);

    public:

    Agent(Processor &processor): processor(processor) {}
    virtual void process_next_message() {}

    virtual void publish(const std::string topic,
                         const json::object& output_message) {}

};

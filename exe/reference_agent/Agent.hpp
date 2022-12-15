#pragma once

#include "Processor.hpp"
#include "Utils.hpp"
#include <boost/json.hpp>
#include <iostream>

namespace json = boost::json;

// interface for write method
class Agent : public Utils {

    // create json objects from text
    json::stream_parser json_parser;

    // count the messages processed and published
    std::vector<std::string> processed_topics, published_topics;

    protected:

    Processor &processor; 
    std::string app_name;

    void log_processed_topic(const std::string topic);
    void log_published_topic(const std::string topic);
    void summarize_activity();
    void configure(const json::object& config);
    json::object parse_json(const std::string text);
    void process(const std::string topic, const json::object& message);

    public:

    Agent(Processor &processor): processor(processor) {}
    virtual void publish(const std::string topic,
                         const json::object& message) {}
};

#pragma once

#include "ReferenceProcessor.hpp"
#include "Utils.hpp"
#include <boost/json.hpp>
#include <iostream>

namespace json = boost::json;

// interface for write method
class Agent : public Utils {

    // only process JSON messages
    json::stream_parser json_parser;

  protected:
    std::string app_name;

    ReferenceProcessor reference_processor = ReferenceProcessor(this);

    json::object parse_json(const std::string text);

    void configure(const json::object& config);

    virtual void start() {}
    virtual void stop() {}

  public:
    virtual void process_next_message() {}
    virtual void publish(json::object& message) {}
};

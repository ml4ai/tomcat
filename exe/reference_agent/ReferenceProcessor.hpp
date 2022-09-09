#pragma once

#include "Processor.hpp"
#include <boost/json.hpp>

namespace json = boost::json;

class Agent;

class ReferenceProcessor : public Processor {

    // Every topic from which a message may be processed
    std::vector<std::string> subscription_topics;

    // Every topic to which a message may be published
    std::vector<std::string> publication_topics;

  public:

    ReferenceProcessor() : Processor() {}

    void configure(const json::object& config, Agent *agent);

    void process_message(const std::string topic,
                         const json::object& message) override;
};

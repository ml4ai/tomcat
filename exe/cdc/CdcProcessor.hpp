#pragma once

#include "Processor.hpp"
#include <boost/json.hpp>

namespace json = boost::json;

class Agent;

class CdcProcessor : public Processor {

    // input is from the Dialog Agent
    const std::string input_topic = "agent/dialog";
    const std::string input_type = "event";
    const std::string input_sub_type = "Event:dialogue_event";

    // output to our own message type
    const std::string output_topic = "agent/tomcat-CDC/coordination_event;
    const std::string output_type = "status";
    const std::string output_sub_type = "Event:dialog_coordination_event";
    

    // topics found in the config file
    std::vector<std::string> input_topics, output_topics;

  public:

    CdcProcessor();

    void configure(const json::object& config, Agent *agent);

    void process_message(const json::object& message) override;
};

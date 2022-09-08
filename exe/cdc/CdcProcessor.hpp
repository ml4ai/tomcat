#pragma once

#include "Processor.hpp"
#include <boost/json.hpp>

namespace json = boost::json;

class Agent;

class CdcProcessor : public Processor {

    // subscription
    const std::string dialog_topic = "agent/dialog";
    const std::string dialog_type = "event";
    const std::string dialog_sub_type = "Event:dialogue_event";
    std::vector<std::string> input_topics;

    // publication
    const std::string cdc_topic = "agent/tomcat-CDC/coordination_event";
    const std::string cdc_type = "status";
    const std::string cdc_sub_type = "Event:dialog_coordination_event";
    std::vector<std::string> output_topics;
    void publish_coordination_message(const json::object &evidence);
    
    // Simple function that allows you to look for a simple label
    bool look_for_label(const json::array &extractions,
		        const std::string label);

  public:

    CdcProcessor() : Processor() {}

    void configure(const json::object& config, Agent *agent);

    void process_message(const json::object& message) override;
};

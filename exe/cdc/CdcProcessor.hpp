#pragma once

#include "Processor.hpp"
#include <boost/json.hpp>

namespace json = boost::json;

class Agent;

class CdcProcessor : public Processor {

    // maintain an array of label sequences to check
    json::array label_pairs = json::array();

    // We use a deque instead of a queue since we will iterate over it.
    size_t utterance_window_size = 5;
    std::deque<json::object> utterance_queue; 

    // subscription
    const std::string dialog_topic = "agent/dialog";
    const std::string dialog_type = "event";
    const std::string dialog_sub_type = "Event:dialogue_event";

    // publication
    const std::string cdc_topic = "agent/tomcat-CDC/coordination_event";
    const std::string cdc_type = "status";
    const std::string cdc_sub_type = "Event:dialog_coordination_event";
    void publish_cdc_message(const json::object &evidence);
    
    // Simple function that allows you to look for a simple label
    bool look_for_label(const json::array &extractions,
		        const std::string label);

    json::object find_evidence(const json::object &input_file); // TODO "file"?

    public:

    CdcProcessor() : Processor() {}

    void configure(const json::object& config, Agent *agent);

    void process_message(const std::string topic,
                         const std::string type,
                         const std::string sub_type,
                         const json::object& message) override;
};

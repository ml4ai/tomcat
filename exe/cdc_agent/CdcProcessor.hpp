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
    
    // Coordination detection
    bool has_label(const json::array &extractions, const std::string label);
    void check_labels(json::array &detected_sequences,
                      const string label1, 
                      const string label2);
    public:

    CdcProcessor() : Processor() {}

    void configure(const json::object& config, Agent *agent);

    void process_message(const std::string topic,
                         const json::object& message) override;
};
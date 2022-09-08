#include "CdcProcessor.hpp"
#include "Agent.hpp"
#include "Processor.hpp"
#include <boost/json.hpp>
#include <boost/json/array.hpp>
#include <boost/log/trivial.hpp>

namespace json = boost::json;


void CdcProcessor::configure(
    const json::object& config,
    Agent *agent) {

    // add input subscriptions
    add_subscription(dialog_topic, dialog_type, dialog_sub_type);

    // add output to publications
    add_publication(cdc_topic, cdc_type, cdc_sub_type);

    Processor::configure(config, agent);
}

// Simple function that allows you to look for a simple label
bool CdcProcessor::look_for_label(
    const json::array &extractions,
    const std::string label) {

    for (auto extraction : extractions) {
        json::array labels = extraction.at("labels").as_array();
        if (contains(labels, json::string_view(label))) {
            return true;
        }
    }
    return false;
}

json::object CdcProcessor::find_evidence(
    const json::object& input_message) {

    json::object evidence = {
        { "evidence", "goes here" }};

    return evidence;
}

// process a Dialog Agent message
void CdcProcessor::process_message(const json::object& input_message) {

    std::string input_topic = val<std::string>(input_message, "topic");

    json::object input_header = val<json::object>(input_message, "header");
    std::string input_type = val<std::string>(input_header, "message_type");

    json::object input_msg = val<json::object>(input_message, "msg");
    std::string input_sub_type = val<std::string>(input_msg, "sub_type");

    // process Dialog Agent Message
    if ((dialog_topic.compare(input_topic) == 0) &&
        (dialog_type.compare(input_type) == 0) &&
        (dialog_sub_type.compare(input_sub_type) == 0)) {

        publish_coordination_message(input_message);
    }

    // forward the message to base class for further processing
    Processor::process_message(input_message);
}

// publish output message
void CdcProcessor::publish_coordination_message(
    const json::object& input_message) {

    const json::object evidence = find_evidence(input_message);

    publish(cdc_topic,
            cdc_type,
            cdc_sub_type,
            input_message,
            evidence);
}

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

    // base class configuration
    Processor::configure(config, agent);

    // add Message Bus info
    add_subscription(dialog_topic, dialog_type, dialog_sub_type);
    add_publication(cdc_topic, cdc_type, cdc_sub_type);

    // read the label pairs to be used in testing
    label_pairs = val<json::array>(config, "check_label_seq");

    // report the label configuration
    std::cout << "CDC Processor test label pairs:" << std::endl;
    for (size_t i = 0; i < label_pairs.size(); i++) {
        json::value value = label_pairs.at(i);
        json::object object = json::value_to<json::object>(value);
        std::string label1 = val<std::string>(object, "label1");
        std::string label2 = val<std::string>(object, "label2");
        std::cout << i << "\t" << label1 << ", " << label2 << std::endl;
    }
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
    const json::object& message) {

    json::object evidence = {
        { "evidence", "goes here" }};

    return evidence;
}

// Process any message
void CdcProcessor::process_message(const std::string topic,
                                   const json::object& message) {

    // base class message processing
    Processor::process_message(topic, message);

    std::string timestamp = get_timestamp();

    json::object header = val<json::object>(message, "header");
    json::object msg = val<json::object>(message, "msg");

    // filter message response by topic, message_type, and sub_type
    std::string message_type = val<std::string>(header, "message_type");
    std::string sub_type = val<std::string>(msg, "sub_type");


    // process Dialog Agent Message
    if ((dialog_topic.compare(topic) == 0) &&
        (dialog_type.compare(message_type) == 0) &&
        (dialog_sub_type.compare(sub_type) == 0)) {

	// make sure participant ID is not "Server"
	const json::object data = val<json::object>(message, "data");
	const std::string participant_id =
            val<std::string>(data, "participant_id", "not_set");
	if(participant_id.compare("Server") == 0) {
	    return;
        }

	// enqueue the message while maintaining the queue size
        if (utterance_queue.size() == utterance_window_size) {
            utterance_queue.pop_front();
        }
        utterance_queue.push_back(message);

	// look for label
	

        publish_cdc_message(message);
    } 
}

// publish output message
void CdcProcessor::publish_cdc_message(const json::object& message) {

    const json::object evidence = find_evidence(message);

}

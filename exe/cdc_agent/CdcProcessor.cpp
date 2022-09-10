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

    // Message Bus info
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

void CdcProcessor::check_labels(json::array &detected_sequences,
		                const string label1, 
				const string label2) {

    // Check first item in the queue for label1
    json::object data1 = val<json::object>(utterance_queue.front(), "data");
    std::string id1 = val<std::string>(data1, "participant_id");
    std::string text1 = val<std::string>(data1, "text");
    json::array extractions1 = val<json::array>(data1, "extractions");

    if(!has_label(extractions1, label1)) {
        return;
    }

    for (size_t i = 1; i < utterance_queue.size(); i++) {
        json::object data2 = val<json::object>(utterance_queue.at(i), "data");
        std::string id2 = val<std::string>(data2, "participant_id");
        std::string text2 = val<std::string>(data2, "text");
        json::array extractions2 = val<json::array>(data2, "extractions");

	if(has_label(extractions2, label2) && (id1 != id2)) {
            json::value sequence = {
	        { "label1", label1 },
	        { "label2", label2 }};

	    detected_sequences.emplace_back(sequence);
        }
    }
}

// Simple function that allows you to look for a simple label
bool CdcProcessor::has_label(const json::array &extractions,
                             const std::string label) {
    for (auto extraction : extractions) {
        json::array labels = extraction.at("labels").as_array();
        if (contains(labels, json::string_view(label))) {
            return true;
        }
    }
    return false;
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
    std::string type = val<std::string>(header, "message_type");
    std::string sub_type = val<std::string>(msg, "sub_type");

    // process Dialog Agent Message
    if ((dialog_topic.compare(topic) == 0) &&
        (dialog_type.compare(type) == 0) &&
        (dialog_sub_type.compare(sub_type) == 0)) {

	// make sure participant ID is not "Server"
	const json::object data = val<json::object>(message, "data");
	const std::string participant_id =
            val<std::string>(data, "participant_id", "not_set");
	if(participant_id.compare("Server") == 0) {
	    return;
        }

	// enqueue the message while maintaining the queue size
        while (utterance_queue.size() >= utterance_window_size) {
            utterance_queue.pop_front();
        }
        utterance_queue.push_back(message);

        json::array detected_sequences;

	// look for label sequences
        for (size_t i = 0; i < label_pairs.size(); i++) {
            json::value value = label_pairs.at(i);
            json::object object = json::value_to<json::object>(value);
            std::string label1 = val<std::string>(object, "label1");
            std::string label2 = val<std::string>(object, "label2");
	    check_labels(detected_sequences, label1, label2);
	}

	// report if we find any
        std::string ts = get_timestamp();
	json::object cdc_message = {
            { "header", new_header(header, ts, cdc_type) },
            { "msg", new_msg(msg, ts, cdc_sub_type) },
            { "data", {
                { "detected_sequences", detected_sequences }}}};

	publish(cdc_topic, cdc_message);
    } 
}

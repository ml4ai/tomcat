#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "ReferenceMessageHandler.hpp"
#include "BaseMessageHandler.hpp"
#include <boost/json/array.hpp>
#include "Agent.hpp"


using namespace std;
namespace json = boost::json;

void ReferenceMessageHandler::configure(const json::object &config) {
    BaseMessageHandler::configure(config);	

    // add subscriptions.  
    input_topics.clear();
    json::array subs = val<json::array>(config, "subscribes");
    for(size_t i = 0 ;  i < subs.size() ; i++) {
	string topic = json::value_to<string>(subs.at(i));
        add_subscription(topic);
	input_topics.push_back(topic);
    }

    // add publications
    output_topics.clear();
    json::array pubs = val<json::array>(config, "publishes");
    for(size_t i = 0 ;  i < pubs.size() ; i++) {
	string topic = json::value_to<string>(pubs.at(i));
        add_publication(topic);
	output_topics.push_back(topic);
    }
}

// process a custom-defined message. 
void ReferenceMessageHandler::process_message(const string input_topic, 
                                              const json::object &input_message) {

    // Process the message if subscribed to the topic
    if(contains(input_topics, input_topic)) {

	// create a data element of any type
	json::object output_data;
	output_data["topic"] = input_topic;
	output_data["text"] = "Hello World!";

        // publish the output message on the output topics
	for(auto &output_topic : output_topics) {
            publish(output_topic, input_message, output_data);
	}
    }

    BaseMessageHandler::process_message(input_topic, input_message);
}

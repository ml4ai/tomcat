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
    json::array subs = val<json::array>(config, "subscribes");
    for(size_t i = 0 ;  i < subs.size() ; i++) {
	string topic = json::value_to<string>(subs.at(i));
        add_subscription(topic);
    }
}



// process a custom-defined message. 
void ReferenceMessageHandler::process_message(const string topic, 
                                              const json::object &message) {

    // define specifics for this handler input
    string input_topic = "reference_agent_input_topic";

    // Process the message if the fields match.  
    if (input_topic.compare("reference_agent_output_topic") == 0) {

        string output_topic = "reference_agent_output_topic";
       
        string timestamp = get_timestamp();

        // create common header
        json::object output_header = create_output_header(
            message,
            timestamp,
            "not_set"
        );

        // create common msg
        json::object output_msg = create_output_msg(
            message,
            timestamp,
            "not_set"
        );

	// create a data element of any type
	json::object output_data;
	output_data["text"] = "Hello World!";

        // assemble outgoing message
        json::object output_message;
        output_message["header"] = output_header;
        output_message["msg"] = output_msg;
        output_message["data"] = output_data;

        // agent takes it from here
        agent->publish(output_topic, output_message);
    }

    BaseMessageHandler::process_message(topic, message);
}

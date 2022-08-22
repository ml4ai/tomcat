#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "ReferenceMessageHandler.hpp"
#include "BaseMessageHandler.hpp"
#include "Agent.hpp"


using namespace std;
namespace json = boost::json;


// process a custom-defined message.  Multiple output messages can be
// generated from a single input message and sent to different topics. 
void ReferenceMessageHandler::process_message(const string topic, 
                                              const json::object &message) {

    // define specifics for this handler input
    string input_topic = "reference_agent_input_topic";
    string input_message_type = "reference_agent_input_message_type";
    string input_sub_type = "reference_agent_input_sub_type";

    // Process the message if the fields match
    if ((input_topic.compare(topic) == 0) &&
        (input_message_type.compare(get_message_type(message)) == 0) &&
        (input_sub_type.compare(get_sub_type(message)) == 0)) {

        string output_topic = "reference_agent_output_topic";
        string output_message_type = "reference_agent_output_message_type";
        string output_sub_type = "reference_agent_output_message_type";
       
        string timestamp = get_timestamp();

        // create common header
        json::object output_header = create_output_header(
            message,
            timestamp,
            output_message_type
        );

        // create common msg
        json::object output_msg = create_output_msg(
            message,
            timestamp,
            output_sub_type
        );

	// create a data element
	json::object output_data;
	output_data["text"] = "Hello World!";

        // assemble outgoing message
        json::object output_message;
        output_message["header"] = output_header;
        output_message["msg"] = output_msg;
        output_message["data"] = output_data;

        // agent takes it from here
        agent->write(output_topic, output_message);
    }


    // propagate unhandled messages to the base class
    else {
        BaseMessageHandler::process_message(topic, message);
    }
}

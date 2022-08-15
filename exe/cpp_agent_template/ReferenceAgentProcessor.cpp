#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "ReferenceAgentProcessor.hpp"


using namespace std;
namespace json = boost::json;

/* Process the input */
void ReferenceAgentProcessor::process_input_message(
    json::object input_header,
    json::object input_msg,
    json::object input_data
) {

    /* message sub_type must match configuration for processing */
    if(!utils.value_matches(input_msg, "sub_type", input_config.sub_type))
    {
        return;
    }


    string timestamp = utils.get_timestamp();

    json::value message = {
        {"header", 
            header(timestamp, input_header)},
	{"msg",
            msg(timestamp, input_msg)},
	{"data",  {
            {"reference_agent_data_key", "reference_agent_data_value"},
	    {"version", version}}}
    };

    publish(message);
}

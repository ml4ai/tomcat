#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "TrialStopProcessor.hpp"

using namespace std;
namespace json = boost::json;

void TrialStopProcessor::process_input_message(
    json::object input_header,
    json::object input_msg,
    json::object input_data
) {
    /* message sub_type must match configuration for processing */
    if(!utils.value_matches(input_msg, "sub_type", input_config.sub_type))
    {
        return;
    }

    cout << "TrialStopProcessor::process_input_message" << endl;


}


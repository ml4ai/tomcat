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
  cout << "TrialStopProcessor::process_input_message" << endl;
}

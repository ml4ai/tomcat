#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "TrialStartProcessor.hpp"

using namespace std;
namespace json = boost::json;

void TrialStartProcessor::process_input_message(
    json::object input_header,
    json::object input_msg,
    json::object input_data
) {
  cout << "TrialStartProcessor::process_input_message" << endl;
}


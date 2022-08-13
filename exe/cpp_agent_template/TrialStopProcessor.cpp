#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "TrialStopProcessor.hpp"

using namespace std;
namespace json = boost::json;

void TrialStopProcessor::process_subscribed_message(
    json::object common_header,
    json::object common_message,
    json::object data
) {
  cout << "TrialStopProcessor::process_subscribed_message" << endl;
}

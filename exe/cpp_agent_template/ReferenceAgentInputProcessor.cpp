#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "ReferenceAgentInputProcessor.hpp"

using namespace std;
namespace json = boost::json;

void ReferenceAgentInputProcessor::process_subscribed_message(
    json::object common_header,
    json::object common_message,
    json::object data
) {
  cout << "ReferenceAgentInputProcessor::process_subscribed_message" << endl;
}

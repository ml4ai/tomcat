#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "ReferenceAgentInputProcessor.hpp"

using namespace std;
namespace json = boost::json;

void ReferenceAgentInputProcessor::process(
    json::object common_header,
    json::object common_message,
    json::object data
) {
  cout << "ReferenceAgentInputProcessor::process" << endl;
}

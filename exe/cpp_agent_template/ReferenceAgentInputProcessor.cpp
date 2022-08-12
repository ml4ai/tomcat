#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "ReferenceAgentInputProcessor.hpp"

using namespace std;
namespace json = boost::json;

void ReferenceAgentInputProcessor::process(json::object read_from_bus) {
  cout << "ReferenceAgentInputProcessor::process" << endl;
}


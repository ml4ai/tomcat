#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "RollcallRequestProcessor.hpp"

using namespace std;
namespace json = boost::json;


void RollcallRequestProcessor::process(
    json::object common_header,
    json::object common_message,
    json::object data
) {
  cout << "RollcallRequestProcessor::process" << endl;
}

#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "RollcallRequestProcessor.hpp"

using namespace std;
namespace json = boost::json;


void RollcallRequestProcessor::process(json::object read_from_bus) {
  cout << "RollcallRequestProcessor::process" << endl;
}

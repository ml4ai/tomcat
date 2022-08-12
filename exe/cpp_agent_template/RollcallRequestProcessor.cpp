#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "RollcallProcessor.hpp"

using namespace std;
namespace json = boost::json;


void RollcallProcessor::process(json::object read_from_bus) {
  cout << "RollcallProcessor::process" << endl;
}

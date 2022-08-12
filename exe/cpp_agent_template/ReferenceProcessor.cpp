#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "ReferenceProcessor.hpp"

using namespace std;
namespace json = boost::json;

void ReferenceProcessor::process(json::object read_from_bus) {
  cout << "ReferenceProcessor::process" << endl;
}


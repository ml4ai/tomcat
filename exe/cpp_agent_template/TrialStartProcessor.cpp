#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "TrialStartProcessor.hpp"

using namespace std;
namespace json = boost::json;

void TrialStartProcessor::process(
    json::object common_header,
    json::object common_message,
    json::object data
) {
  cout << "TrialStartProcessor::process" << endl;
}


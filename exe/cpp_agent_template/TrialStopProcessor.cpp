#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "TrialStopProcessor.hpp"

using namespace std;
namespace json = boost::json;


string TrialStopProcessor::get_name() { 
    return "trial_stop";
}


#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "TrialMessageHandler.hpp"
#include "Agent.hpp"

using namespace std;
namespace json = boost::json;


void TrialMessageHandler::configure(json::object config, Agent *agent) {

    MessageHandler::configure(config, agent);    

    // Version Info data.  Never changes.
    data = config;
}

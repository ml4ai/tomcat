#include <boost/json.hpp>
#include <boost/log/trivial.hpp>
#include "Agent.hpp"
#include "ReferenceMessageHandler.hpp"
#include <iostream>


using namespace std;
namespace json = boost::json;

Agent::Agent(const json::object &config) {
    version = json::value_to<string>(config.at("version"));
}

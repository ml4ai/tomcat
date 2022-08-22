#pragma once

#include <boost/json.hpp>
#include "ReferenceMessageHandler.hpp"

namespace json = boost::json;

using namespace std;


// interface for write method
class Agent {

    protected:


    ReferenceMessageHandler message_handler = ReferenceMessageHandler(this);


    string version;

    Agent(const json::object &config);

    public:

    virtual void write(const string topic, json::object &message) = 0;
};

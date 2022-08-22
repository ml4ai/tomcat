#pragma once

#include <boost/json.hpp>
#include "ReferenceMessageHandler.hpp"

namespace json = boost::json;

using namespace std;


// interface for write method
class Agent {

    
    ReferenceMessageHandler message_handler = ReferenceMessageHandler(this);

    protected:



    void process_message(const string topic, const json::object &message);

    string version;

    Agent(const json::object &config);

    vector<string>get_input_topics();

    vector<string>get_output_topics();

    public:


    virtual void publish(const string topic, json::object &message) = 0;
};

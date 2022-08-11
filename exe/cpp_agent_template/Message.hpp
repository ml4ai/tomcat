#pragma once

#include <string>
#include <boost/json.hpp>

namespace json = boost::json;
using namespace std;

/** Testbed Message base class */
class Message{

    public:
	string name = "not set";  // Arbitrary for this code only
	string message_type;   // header.message_type
	string sub_type;   // msg.sub_type
	string version;  // this software version, from config file
        string topic;   // message bus topic

	// Constructor
	Message(string name, json::object config);

};

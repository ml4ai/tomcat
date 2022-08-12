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

    struct CommonHeader {
        string timestamp;
        string message_type;
	string version;  // testbed version
    };

    struct CommonMsg {
        string experiment_id;
        string timestamp;
        string source;
        string sub_type;
        string version;  // this software version
    };
};

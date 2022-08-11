#pragma once

#include <string>
#include <boost/json.hpp>
#include "Message.hpp"

namespace json = boost::json;
using namespace std;


/** This base class is for messages published to the Message Bus */
class PublishedMessage: public Message {

    public:

    /** publication source */
    string source;

    /** Constructor */
    PublishedMessage(string name, json::object config);

    /** Destructor */
    ~PublishedMessage();

    /** Data are unique for each message type, subclasses generate this */
    virtual json::value data_json_value();

    /** JSON serialization */
    json::value json_value(string timestamp);
};

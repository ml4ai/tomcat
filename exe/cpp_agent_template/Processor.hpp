#pragma once

#include <string>
#include <boost/json.hpp>
#include "Message.hpp"

namespace json = boost::json;

using namespace std;


/** A base class for subscribed message handlers */
class Processor : Message{

  public:

    /** Constructor */
    Processor(string name, json::object config);

    /** handler call */
    virtual void handle_message(string topic, json::object message) {};

    /** processor call */
};

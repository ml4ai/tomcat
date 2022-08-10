#pragma once

#include <string>
#include <boost/json.hpp>
#include "Message.hpp"

namespace json = boost::json;
using namespace std;


/** A base class for subscribed message handlers */
class Processor : public Message{

  public:

    /** Constructor */
    Processor(string name, json::object config);

    /** handle message */
    virtual void process(string topic, json::object message) {};

    /** processor call */
};

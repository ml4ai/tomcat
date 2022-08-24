#pragma once

#include <string>
#include <boost/json.hpp>
#include <mqtt/async_client.h>


namespace json = boost::json;

using namespace std;


/** Maintain a FIFO queue of messages */
class MessageQueue {

    int count = 0;  // elements in queue 

    public:

    // should take the processor as an arg
    // processor should support a busy(): bool call
    MessageQueue(){}

    bool enqueue(const json::object *ptr);
    json::object* dequeue();

    bool empty();
    int size();

    // should enqueue messages
  
};

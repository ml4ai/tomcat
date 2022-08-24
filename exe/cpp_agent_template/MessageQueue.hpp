#pragma once

#include <string>
#include <boost/json.hpp>
#include <mqtt/async_client.h>


namespace json = boost::json;

using namespace std;


/** Maintain a FIFO queue of messages */
class MessageQueue {

    public:

    // should take the processor as an arg
    // processor should support a busy(): bool call
    MessageQueue();

    bool enqueue(mqtt::const_message_ptr *ptr);


    // should enqueue messages
  
};

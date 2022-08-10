#pragma once

#include <string>
#include <boost/json.hpp>


namespace json = boost::json;

using namespace std;


/** Maintain a FIFO queue of messages */
class MessageQueue {

    public:

    // should take the processor as an arg
    // processor should support a busy(): bool call
    MessageQueue();


    // should enqueue messages
  
};

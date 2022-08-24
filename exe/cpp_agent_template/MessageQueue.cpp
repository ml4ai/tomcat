#include <boost/thread/thread.hpp>
#include <boost/lockfree/queue.hpp>
#include <iostream>
#include <boost/json.hpp>
#include <boost/atomic.hpp>
#include <mqtt/async_client.h>


#include "MessageQueue.hpp"

boost::atomic_int producer_count(0);
boost::atomic_int consumer_count(0);

boost::lockfree::queue<mqtt::const_message_ptr*> my_queue(128);

const int iterations = 10000000;
const int producer_thread_count = 4;
const int consumer_thread_count = 4;

bool MessageQueue::enqueue(mqtt::const_message_ptr *ptr) {
   return my_queue.push(ptr);
}



/*
void producer(void)
{
    for (int i = 0; i != iterations; ++i) {
        int value = ++producer_count;
        while (!my_queue.push(value))
            ;
    }
}

boost::atomic<bool> done (false);
void consumer(void)
{
    int value;
    while (!done) {
        while (my_queue.pop(value))
            ++consumer_count;
    }

    while (my_queue.pop(value))
        ++consumer_count;
}
*/

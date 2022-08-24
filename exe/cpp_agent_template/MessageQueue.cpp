#include <boost/thread/thread.hpp>
#include <boost/lockfree/queue.hpp>
#include <iostream>
#include <boost/json.hpp>
#include <boost/atomic.hpp>
#include <mqtt/async_client.h>


#include "MessageQueue.hpp"

// Further reading:  
// https://www.boost.org/doc/libs/1_54_0/doc/html/boost/lockfree/queue.html
// https://www.boost.org/doc/libs/1_54_0/doc/html/lockfree/examples.html

/*
boost::atomic_int producer_count(0);
boost::atomic_int consumer_count(0);
*/

boost::lockfree::queue<json::object*> my_queue(128);

/*
const int iterations = 10000000;
const int producer_thread_count = 4;
const int consumer_thread_count = 4;
*/

bool MessageQueue::enqueue(json::object *ptr) {
    cout << "MessageQueue::enqueue" << endl;
    bool ret =  my_queue.push(ptr);
    if(ret) {
        count ++;
        cout << "Queue push. Size = " << count << endl;
    } else {
        cerr << "Queue push failed" << endl;
    }
    return ret;
}

json::object* MessageQueue::dequeue() {
    cout << "MessageQueue::enqueue" << endl;

    json::object* value;

    my_queue.pop(value);
    count --;
    cout << "Queue pop. Size = " << count << endl;

    return value;
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

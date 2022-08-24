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

boost::lockfree::queue<const json::object*> q(128);

/*
const int iterations = 10000000;
const int producer_thread_count = 4;
const int consumer_thread_count = 4;
*/

bool MessageQueue::enqueue(const json::object *ptr) {
    cout << "MessageQueue::enqueue" << endl;
    bool ret =  q.push(ptr);
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

    q.pop(value);
    count --;
    cout << "Queue pop. Size = " << count << endl;

    return value;
}

bool MessageQueue::empty() {
    return q.empty();
}

int MessageQueue::size() {
    return count;
}



/*
void producer(void)
{
    for (int i = 0; i != iterations; ++i) {
        int value = ++producer_count;
        while (!q.push(value))
            ;
    }
}

boost::atomic<bool> done (false);
void consumer(void)
{
    int value;
    while (!done) {
        while (q.pop(value))
            ++consumer_count;
    }

    while (q.pop(value))
        ++consumer_count;
}
*/

#pragma once

#include <mutex>
#include <queue>

#include "utils/Definitions.h"

namespace tomcat {
    namespace model {

        /**
         * Basic class for thread-safe queue that can be written and consumed by
         * different threads.
         */
        template <typename T> class SynchronizedQueue {
          public:
            SynchronizedQueue() {}

            ~SynchronizedQueue() {}

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------
            SynchronizedQueue(const SynchronizedQueue&) = delete;

            SynchronizedQueue& operator=(const SynchronizedQueue&) = delete;

            SynchronizedQueue(SynchronizedQueue&&) = delete;

            SynchronizedQueue& operator=(SynchronizedQueue&&) = delete;

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------
            void push(const T& data) {
                std::scoped_lock lock(this->mutex);
                this->queue.push(data);
            }

            bool empty() const {
                std::scoped_lock lock(this->mutex);
                return this->queue.empty();
            }

            T& front() {
                std::scoped_lock lock(this->mutex);
                return this->queue.front();
            }

            T const& front() const {
                std::scoped_lock lock(this->mutex);
                return this->queue.front();
            }

            void pop() {
                std::scoped_lock lock(this->mutex);
                this->queue.pop();
            }

          private:
            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------
            std::queue<T> queue;
            mutable std::mutex mutex;
        };

    } // namespace model
} // namespace tomcat

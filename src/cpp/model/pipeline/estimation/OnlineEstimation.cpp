#include "OnlineEstimation.h"

#include <chrono>
#include <thread>

namespace tomcat {
    namespace model {

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        OnlineEstimation::OnlineEstimation(std::shared_ptr<Estimator> estimator,
                                           MessageBrokerConfiguration& config)
            : Estimation(estimator), config(config) {}

        OnlineEstimation::OnlineEstimation(std::shared_ptr<Estimator> estimator,
                                           MessageBrokerConfiguration&& config)
            : Estimation(estimator), config(std::move(config)) {}

        OnlineEstimation::~OnlineEstimation() {}

        //----------------------------------------------------------------------
        // Copy & Move constructors/assignments
        //----------------------------------------------------------------------
        OnlineEstimation::OnlineEstimation(const OnlineEstimation& estimation) {
            this->copy_estimation(estimation);
        }

        OnlineEstimation&
        OnlineEstimation::operator=(const OnlineEstimation& estimation) {
            this->copy_estimation(estimation);
            return *this;
        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void OnlineEstimation::reset() {
            Estimation::reset();
            this->init = false;
        }

        void
        OnlineEstimation::copy_estimation(const OnlineEstimation& estimation) {
            Estimation::copy_estimation(estimation);
            this->config = estimation.config;
        }

        void OnlineEstimation::estimate(EvidenceSet test_data) {
            // TODO - start new connection with the message broker and listens
            //  to it forever (it's killed externally).
            // This is temporary. Just to test the thread creation

            this->estimator->estimate(test_data);
            std::this_thread::sleep_for (std::chrono::seconds(1));

            if (this->config.timeout > 0) {
                if (!this->init) {
                    this->last_updated_time = std::chrono::steady_clock::now();
                    this->init = true;
                }
                else {
                    Time current_time = std::chrono::steady_clock::now();
                    long duration =
                        std::chrono::duration_cast<std::chrono::seconds>(
                            current_time - this->last_updated_time)
                            .count();

                    if (duration > this->config.timeout) {
                        this->finished = true;
                    }
                }
            }
        }

    } // namespace model
} // namespace tomcat

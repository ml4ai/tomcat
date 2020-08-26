#include "OnlineEstimation.h"

#include <sstream>
#include <thread>

#include "../../utils/EigenExtensions.h"

namespace tomcat {
    namespace model {

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        OnlineEstimation::OnlineEstimation(std::shared_ptr<Estimator> estimator,
                                           MessageBrokerConfiguration config)
            : Estimation(estimator), config(config) {}

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
            this->time_step = 0;
        }

        void
        OnlineEstimation::copy_estimation(const OnlineEstimation& estimation) {
            Estimation::copy_estimation(estimation);
            Mosquitto::copy_wrapper(estimation);
            this->config = estimation.config;
        }

        void OnlineEstimation::estimate(EvidenceSet test_data) {
            this->set_max_seconds_without_messages(this->config.timeout);
            this->connect(this->config.address, this->config.port, 60);
            this->subscribe(this->config.state_topic);
            this->subscribe(this->config.events_topic);
            this->loop();
            this->close();
            this->finished = true;

            //            this->estimator->estimate(test_data);
            //            std::this_thread::sleep_for(std::chrono::seconds(1));
            //
            //            if (this->config.timeout > 0) {
            //                if (!this->init) {
            //                    this->last_updated_time =
            //                    std::chrono::steady_clock::now(); this->init =
            //                    true;
            //                }
            //                else {
            //                    Time current_time =
            //                    std::chrono::steady_clock::now(); long
            //                    duration =
            //                        std::chrono::duration_cast<std::chrono::seconds>(
            //                            current_time -
            //                            this->last_updated_time) .count();
            //
            //                    if (duration > this->config.timeout) {
            //                        this->finished = true;
            //                    }
            //                }
            //            }
        }

        void OnlineEstimation::on_error(const std::string& error_message) {
            this->close();
            throw TomcatModelException(error_message);
        }

        void OnlineEstimation::on_message(const std::string& topic,
                                          const std::string& message) {
            // Convert message to data set.
            Tensor3 data1(Eigen::MatrixXd::Ones(1, 1));
            Tensor3 data2(Eigen::MatrixXd::Ones(1, 1));
            EvidenceSet new_data;
            new_data.add_data("TG", data1);
            new_data.add_data("TY", data2);
            this->estimator->estimate(new_data);

            try {
                for (const auto& node_estimates :
                     this->estimator->get_estimates_at(this->time_step)) {
                    std::stringstream ss_topic;
                    ss_topic << "estimates/" << node_estimates.label;

                    this->publish(ss_topic.str(),
                                  to_string(node_estimates.estimates));
                }
            }
            catch (std::out_of_range& e) {
                this->publish("log", "max_time_step_reached");
                this->running = false;
            }
            this->time_step++;
        }

        void OnlineEstimation::on_time_out() {
            this->publish("log", "time_out");
        }

    } // namespace model
} // namespace tomcat

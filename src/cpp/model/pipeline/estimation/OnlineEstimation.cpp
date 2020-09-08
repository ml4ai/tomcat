#include "OnlineEstimation.h"

#include <algorithm>
#include <sstream>
#include <thread>

#include "../../utils/EigenExtensions.h"

namespace tomcat {
    namespace model {

        using namespace std;

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        OnlineEstimation::OnlineEstimation(MessageBrokerConfiguration config)
            : config(config) {}

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
            EstimationProcess::reset();
            this->time_step = 0;
        }

        void
        OnlineEstimation::copy_estimation(const OnlineEstimation& estimation) {
            EstimationProcess::copy_estimation(estimation);
            Mosquitto::copy_wrapper(estimation);
            this->config = estimation.config;
        }

        void OnlineEstimation::estimate(EvidenceSet test_data) {
            this->set_max_seconds_without_messages(this->config.timeout);
            this->connect(this->config.address, this->config.port, 60);
            this->subscribe(this->config.state_topic);
            this->subscribe(this->config.events_topic);
            thread estimation_thread(
                &OnlineEstimation::run_estimation_thread, this);
            this->loop();
            this->close();
            // Join because even if messages are not coming anymore, pending
            // data from previous messages could still be in the queue to
            // be processed.
            estimation_thread.join();
        }

        void OnlineEstimation::run_estimation_thread() {
            while (this->running || !this->data_to_process.empty()) {
                // To avoid the overload of creating and destroying threads, so
                // far the estimators will run in sequence. Later this can be
                // improved by creating perennial threads for each one of the
                // estimators and keep tracking of the data point they have
                // processes in the list of available data.
                if (!this->data_to_process.empty()) {
                    EvidenceSet new_data = this->data_to_process.front();
                    this->data_to_process.pop();
                    for (auto estimator : this->estimators) {
                        estimator->estimate(new_data);
                    }

                    this->publish_last_estimates();
                    this->time_step++;
                }
            }
        }

        void OnlineEstimation::publish_last_estimates() {
            try {
                for (const auto estimator : this->estimators) {
                    for (const auto& node_estimates :
                         estimator->get_estimates_at(this->time_step)) {
                        string estimator_name = estimator->get_name();
                        replace(estimator_name.begin(),
                                     estimator_name.end(),
                                     ' ',
                                     '_');
                        stringstream ss_topic;
                        ss_topic << this->config.estimates_topic << "/"
                                 << estimator_name << "/"
                                 << node_estimates.label;

                        this->publish(ss_topic.str(),
                                      to_string(node_estimates.estimates));
                    }
                }
            }
            catch (out_of_range& e) {
                this->publish(this->config.log_topic, "max_time_step_reached");
                this->running = false;
            }
        }

        void OnlineEstimation::on_error(const string& error_message) {
            this->close();
            throw TomcatModelException(error_message);
        }

        void OnlineEstimation::on_message(const string& topic,
                                          const string& message) {
            // TODO: Convert message to data set.
            Tensor3 data1(Eigen::MatrixXd::Ones(1, 1));
            Tensor3 data2(Eigen::MatrixXd::Ones(1, 1));
            EvidenceSet new_data;
            new_data.add_data("TG", data1);
            new_data.add_data("TY", data2);
            this->data_to_process.push(new_data);
        }

        void OnlineEstimation::on_time_out() {
            this->publish(this->config.log_topic, "time_out");
        }

    } // namespace model
} // namespace tomcat

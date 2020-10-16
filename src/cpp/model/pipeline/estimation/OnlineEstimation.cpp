#include "OnlineEstimation.h"

#include <algorithm>
#include <sstream>
#include <thread>
#include <unordered_map>

#include <nlohmann/json.hpp>

#include "utils/EigenExtensions.h"

namespace tomcat {
    namespace model {

        using namespace std;

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        OnlineEstimation::OnlineEstimation(
            MessageBrokerConfiguration config,
            std::shared_ptr<MessageConverter> message_converter)
            : config(config), message_converter(message_converter) {}

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
        void OnlineEstimation::prepare() {
            EstimationProcess::prepare();
            this->time_step = 0;
        }

        void
        OnlineEstimation::copy_estimation(const OnlineEstimation& estimation) {
            EstimationProcess::copy_estimation(estimation);
            Mosquitto::copy_wrapper(estimation);
            this->config = estimation.config;
            this->message_converter = estimation.message_converter;
        }

        void OnlineEstimation::estimate(const EvidenceSet& test_data) {
            this->set_max_seconds_without_messages(this->config.timeout);
            this->connect(this->config.address, this->config.port, 60);
            this->subscribe(this->config.state_topic);
            this->subscribe(this->config.events_topic);
            this->subscribe(this->config.trial_topic);
            thread estimation_thread(&OnlineEstimation::run_estimation_thread,
                                     this);
            this->loop();
            this->close();
            // Join because even if messages are not coming anymore, pending
            // data from previous messages could still be in the queue to
            // be processed.
            estimation_thread.join();
        }

        void OnlineEstimation::run_estimation_thread() {
            while (this->running || !this->messages_to_process.empty()) {
                // To avoid the overload of creating and destroying threads, so
                // far the estimators will run in sequence. Later this can be
                // improved by creating perennial threads for each one of the
                // estimators and keep tracking of the data point they have
                // processed in the list of available data.
                EvidenceSet new_data = get_next_data_from_pending_messages();
                if (!new_data.empty()) {
                    for (auto estimator : this->estimators) {
                        EstimationProcess::estimate(estimator, new_data);
                    }

                    this->publish_last_estimates();
                    this->time_step++;
                }
            }
        }

        EvidenceSet OnlineEstimation::get_next_data_from_pending_messages() {
            EvidenceSet new_data;

            while (!this->messages_to_process.empty() && new_data.empty()) {
                nlohmann::json message = this->messages_to_process.front();
                this->messages_to_process.pop();
                unordered_map<string, double> observations_per_node =
                    this->message_converter->convert_online(message);
                if (!observations_per_node.empty()) {
                    for (const auto& [node_label, value] :
                         observations_per_node) {
                        Tensor3 data(Eigen::MatrixXd::Constant(1, 1, value));
                        new_data.add_data(node_label, data);
                    }
                }
            }

            return new_data;
        }

        void OnlineEstimation::publish_last_estimates() {
            try {
                for (const auto estimator : this->estimators) {
                    NodeEstimates estimates =
                        estimator->get_estimates_at(this->time_step);
                    string estimator_name = estimator->get_name();
                    replace(
                        estimator_name.begin(), estimator_name.end(), ' ', '_');
                    stringstream ss_topic;

                    Eigen::VectorXd estimates_vector(
                        estimates.estimates.size());

                    int i = 0;
                    for (const auto& estimates : estimates.estimates) {
                        estimates_vector[i] = estimates(0, 0);
                    }

                    ss_topic << this->config.estimates_topic << "/"
                             << estimator_name << "/" << estimates.label;

                    this->publish(ss_topic.str(), to_string(estimates_vector));

                    //                        if
                    //                        (node_estimates.assignment.size()
                    //                        == 0) {
                    //                            // There will be estimates for
                    //                            each one of the possible
                    //                            node's assignments. We publish
                    //                            each
                    //                            // estimate in a different
                    //                            topic. for (int assignment =
                    //                            0; assignment <
                    //                            node_estimates.estimates.size();
                    //                            assignment++) {
                    //                                ss_topic <<
                    //                                this->config.estimates_topic
                    //                                << "/"
                    //                                         << estimator_name
                    //                                         << "/"
                    //                                         <<
                    //                                         node_estimates.label
                    //                                         << "/"
                    //                                         << assignment;
                    //
                    //                                this->publish(ss_topic.str(),
                    //                                              to_string(node_estimates.estimates[assignment]));
                    //                            }
                    //                        } else {
                    //                            // Use the fixed assignment as
                    //                            a topic ss_topic <<
                    //                            this->config.estimates_topic
                    //                            << "/"
                    //                                     << estimator_name <<
                    //                                     "/"
                    //                                     <<
                    //                                     node_estimates.label
                    //                                     << "/"
                    //                                     <<
                    //                                     node_estimates.assignment;
                    //
                    //                            this->publish(ss_topic.str(),
                    //                                          to_string(node_estimates.estimates[0]));
                    //                        }
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

            nlohmann::json json_message = nlohmann::json::parse(message);
            json_message["topic"] = topic;
            this->messages_to_process.push(json_message);
        }

        void OnlineEstimation::on_time_out() {
            this->publish(this->config.log_topic, "time_out");
        }

        void OnlineEstimation::get_info(nlohmann::json& json) const {
            EstimationProcess::get_info(json);
            json["process"] = "online";
        }

    } // namespace model
} // namespace tomcat

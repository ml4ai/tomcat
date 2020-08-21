#include "BaselineEstimator.h"

#include <iostream>

namespace tomcat {
    namespace model {

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        BaselineEstimator::BaselineEstimator(
            std::shared_ptr<DynamicBayesNet> model, int inference_horizon)
            : Estimator(model, inference_horizon) {}

        BaselineEstimator::~BaselineEstimator() {}

        //----------------------------------------------------------------------
        // Copy & Move constructors/assignments
        //----------------------------------------------------------------------
        BaselineEstimator::BaselineEstimator(
            const BaselineEstimator& estimator) {
            this->copy_estimator(estimator);
        }

        BaselineEstimator&
        BaselineEstimator::operator=(const BaselineEstimator& estimator) {
            this->copy_estimator(estimator);
            return *this;
        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void BaselineEstimator::estimate(DBNData new_data) {
            for (const auto& node_label : this->node_labels) {
                if (EXISTS(node_label, this->nodes_to_reestimate)) {
                    if (!this->training_data.has_data_for(node_label)) {
                        throw TomcatModelException(
                            "No data was provided to the node " + node_label);
                    }

                    // We repeat the frequencies for all of the data points in
                    // the test set.
                    Tensor3 data =
                        this->training_data[node_label].mean(1).repeat(
                            new_data.get_num_data_points(), 1);
                    this->estimates.set_data_for(node_label, data);
                    this->nodes_to_reestimate.erase(node_label);
                }
            }
        }

        DBNData
        BaselineEstimator::get_last_estimates(int initial_time_step) const {
            DBNData sliced_estimates;
            for (const auto& node_label : this->node_labels) {
                sliced_estimates.add_data(
                    node_label,
                    this->estimates[node_label].slice(
                        initial_time_step, Tensor3::ALL, 2));
            }

            return sliced_estimates;
        }

        void BaselineEstimator::add_node(const std::string node_label) {
            Estimator::add_node(node_label);
            this->nodes_to_reestimate.insert(node_label);
        }

        void
        BaselineEstimator::set_training_data(const DBNData& training_data) {
            Estimator::set_training_data(training_data);
            this->nodes_to_reestimate.insert(this->node_labels.begin(),
                                             this->node_labels.end());
        }

    } // namespace model
} // namespace tomcat

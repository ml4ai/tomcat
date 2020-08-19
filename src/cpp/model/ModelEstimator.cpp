#include "ModelEstimator.h"

namespace tomcat {
    namespace model {

        //----------------------------------------------------------------------
        // Definitions
        //----------------------------------------------------------------------

        // No definitions in this file

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        ModelEstimator::ModelEstimator() {}

        ModelEstimator::ModelEstimator(std::shared_ptr<DynamicBayesNet> model,
                                       int inference_horizon)
            : model(model), inference_horizon(inference_horizon) {}

        ModelEstimator::~ModelEstimator() {}

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void ModelEstimator::copy_estimator(const ModelEstimator& estimator) {
            this->model = estimator.model;
            this->training_data = estimator.training_data;
            this->test_data = estimator.test_data;
            this->node_labels = estimator.node_labels;
            this->last_processed_time_step = estimator.last_processed_time_step;
            this->inference_horizon = estimator.inference_horizon;
        }

        void ModelEstimator::add_node(const std::string node_label) {
            this->node_labels.insert(node_label);
        }

        //----------------------------------------------------------------------
        // Getters & Setters
        //----------------------------------------------------------------------
        void
        ModelEstimator::set_training_data(const EvidenceSet& training_data) {
            this->training_data = training_data;
        }

    } // namespace model
} // namespace tomcat

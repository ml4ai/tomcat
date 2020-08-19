#include "Estimator.h"

namespace tomcat {
    namespace model {

        //----------------------------------------------------------------------
        // Definitions
        //----------------------------------------------------------------------

        // No definitions in this file

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        Estimator::Estimator() {}

        Estimator::Estimator(std::shared_ptr<DynamicBayesNet> model,
                                       int inference_horizon)
            : model(model), inference_horizon(inference_horizon) {}

        Estimator::~Estimator() {}

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void Estimator::copy_estimator(const Estimator& estimator) {
            this->model = estimator.model;
            this->training_data = estimator.training_data;
            this->test_data = estimator.test_data;
            this->node_labels = estimator.node_labels;
            this->last_processed_time_step = estimator.last_processed_time_step;
            this->inference_horizon = estimator.inference_horizon;
        }

        void Estimator::add_node(const std::string node_label) {
            this->node_labels.insert(node_label);
        }

        //----------------------------------------------------------------------
        // Getters & Setters
        //----------------------------------------------------------------------
        void Estimator::set_training_data(const EvidenceSet& training_data) {
            this->training_data = training_data;
        }

    } // namespace model
} // namespace tomcat

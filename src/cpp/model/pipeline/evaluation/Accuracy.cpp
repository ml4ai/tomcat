#include "Accuracy.h"

namespace tomcat {
    namespace model {

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        Accuracy::Accuracy(std::shared_ptr<Estimator> estimator)
            : Measure(estimator) {}

        Accuracy::~Accuracy() {}

        //----------------------------------------------------------------------
        // Copy & Move constructors/assignments
        //----------------------------------------------------------------------
        Accuracy::Accuracy(const Accuracy& accuracy) {
            this->copy_measure(accuracy);
        }

        Accuracy& Accuracy::operator=(const Accuracy& accuracy) {
            this->copy_measure(accuracy);
            return *this;
        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        std::vector<NodeEvaluation>
        Accuracy::evaluate(const EvidenceSet& test_data) const {
            std::vector<NodeEvaluation> evaluations;

            for (const auto& estimates :
                 this->estimator->get_last_estimates(0)) {
                NodeEvaluation evaluation;
                evaluation.label = estimates.label;
                evaluation.assignment = estimates.assignment;

                Eigen::MatrixXd ones =
                    Eigen::MatrixXd::Ones(estimates.estimates.rows(),
                                          estimates.estimates.cols());
                Eigen::MatrixXd zeros =
                    Eigen::MatrixXd::Zero(estimates.estimates.rows(),
                                          estimates.estimates.cols());

                Eigen::MatrixXd discrete_estimates =
                    (estimates.estimates.array() > 0.5).select(ones, zeros);

                Eigen::MatrixXd observed_data_in_horizon =
                    EvidenceSet::get_observations_in_window(
                        test_data[estimates.label],
                        Eigen::VectorXd::Constant(1, 1),
                        this->estimator->get_inference_horizon());

                int node_initial_time =
                    EvidenceSet::get_first_time_with_observation(
                        test_data[estimates.label]);
                int total = 0;
                int hits = 0;
                int rows = discrete_estimates.rows();
                int cols = discrete_estimates.cols();

                LOG(discrete_estimates);
                LOG(observed_data_in_horizon);
                for (int i = node_initial_time; i < rows; i++) {
                    for (int j = node_initial_time; j < cols; j++) {
                        if (discrete_estimates(i, j) ==
                            observed_data_in_horizon(i, j)) {
                            hits++;
                        }
                        total++;
                    }
                }

                evaluation.evaluation =
                    Eigen::MatrixXd::Constant(1, 1, (double)hits / total);
                evaluations.push_back(evaluation);
            }

            return evaluations;
        }

        void Accuracy::get_info(nlohmann::json& json) const {
            json["name"] = "accuracy";
            this->estimator->get_info(json["estimator"]);
        }

    } // namespace model
} // namespace tomcat

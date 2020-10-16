#include "Measure.h"

namespace tomcat {
    namespace model {

        using namespace std;

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        Measure::Measure() {}

        Measure::Measure(shared_ptr<Estimator> estimator,
                         double threshold,
                         bool use_last_estimate)
            : estimator(estimator), threshold(threshold),
              use_last_estimate(use_last_estimate) {}

        Measure::~Measure() {}

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void Measure::copy_measure(const Measure& measure) {
            this->estimator = measure.estimator;
            this->threshold = measure.threshold;
            this->use_last_estimate = measure.use_last_estimate;
        }

        ConfusionMatrix
        Measure::get_confusion_matrix(const Eigen::MatrixXd& probabilities,
                                      const Eigen::MatrixXd& true_values,
                                      int fixed_assignment) const {
            // Since this method assumes the estimates were computed for a fixed
            // node assignment, we can safely use the first element of the
            // estimates.estimates vector, as there will be only one element in
            // there.
            Eigen::MatrixXd ones = Eigen::MatrixXd::Ones(probabilities.rows(),
                                                         probabilities.cols());
            Eigen::MatrixXd zeros = Eigen::MatrixXd::Zero(probabilities.rows(),
                                                          probabilities.cols());

            // Preserve the first time steps with no observed values for the
            // estimate in analysis.
            Eigen::MatrixXd no_obs =
                Eigen::MatrixXd::Constant(probabilities.rows(),
                                          probabilities.cols(),
                                          NO_OBS);
            no_obs = (probabilities.array() == NO_OBS)
                         .select(no_obs, zeros);

            Eigen::MatrixXd discrete_estimates =
                (probabilities.array() > this->threshold)
                    .select(ones, no_obs);

            // For a given assignment, transform the test data into 0s and 1s.
            // Where 1 is assigned to the coefficients equal to the assignment
            // informed and 0 otherwise.
            Eigen::MatrixXd observed_data_in_horizon =
                EvidenceSet::get_observations_in_window(
                    true_values,
                    Eigen::VectorXd::Constant(1, fixed_assignment),
                    this->estimator->get_inference_horizon());

            // The first columns with NO_OBS value must not be counted as this
            // means the node does not exist in the time step represented by
            // that column.
            int node_initial_time =
                EvidenceSet::get_first_time_with_observation(
                    observed_data_in_horizon);

            int rows = observed_data_in_horizon.rows();
            int cols = observed_data_in_horizon.cols();
            ConfusionMatrix confusion_matrix;
            for (int i = 0; i < rows; i++) {
                for (int j = node_initial_time; j < cols; j++) {
                    if (discrete_estimates(i, j) == 0 &&
                        observed_data_in_horizon(i, j) == 0) {
                        confusion_matrix.true_negatives++;
                    }
                    else if (discrete_estimates(i, j) == 0 &&
                             observed_data_in_horizon(i, j) == 1) {
                        confusion_matrix.false_negatives++;
                    }
                    else if (discrete_estimates(i, j) == 1 &&
                             observed_data_in_horizon(i, j) == 0) {
                        confusion_matrix.false_positives++;
                    }
                    else {
                        confusion_matrix.true_positives++;
                    }
                }
            }

            return confusion_matrix;
        }

    } // namespace model
} // namespace tomcat

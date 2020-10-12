#include "Accuracy.h"

namespace tomcat {
    namespace model {

        using namespace std;

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        Accuracy::Accuracy(shared_ptr<Estimator> estimator,
                           double threshold,
                           bool use_last_estimate)
            : Measure(estimator, threshold, use_last_estimate) {}

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
        vector<NodeEvaluation>
        Accuracy::evaluate(const EvidenceSet& test_data) const {
            vector<NodeEvaluation> evaluations;

            for (const auto& estimates : this->estimator->get_estimates()) {
                NodeEvaluation evaluation;
                evaluation.label = estimates.label;
                evaluation.assignment = estimates.assignment;

                double accuracy = 0;
                if (estimates.assignment.size() == 0) {
                    // In this case, the estimates are probabilities for each
                    // one of the possible assignments a node can take. So we
                    // need to get the highest probability to decide the
                    // estimated assignment and then compare it against the true
                    // assignment.

                    // Get matrix of true observations starting from the first
                    // time step wth observed data.
                    Tensor3 real_data_3d = test_data[evaluation.label];
                    int first_valid_time_step =
                        EvidenceSet::get_first_time_with_observation(
                            real_data_3d);
                    Eigen::MatrixXd real_data = real_data_3d(0, 0);

                    int num_right_inferences = 0;
                    int total_inferences = 0;
                    int cols = estimates.estimates[0].cols();
                    int t0 = this->use_last_estimate ? (cols - 1) : first_valid_time_step;
                    for (int d = 0; d < test_data.get_num_data_points(); d++) {
                        for (int t = t0; t < cols; t++) {
                            // Each element of the vector estimates.estimates
                            // represents a possible assignment a node can have
                            // (0, 1, ..., cardinality - 1), and it contains a
                            // matrix of estimated probability for such
                            // assignment, where the rows index a data point and
                            // the columns index a time step. We loop over the
                            // elements of the vector for a given data point and
                            // time, to compute the estimated assignment by
                            // choosing the one with highest probability among
                            // the other assignments' estimate for the same data
                            // point and time step.
                            int inferred_assignment = 0;
                            double max_prob = 0;
                            for (int i = 0; i < estimates.estimates.size();
                                 i++) {
                                double prob = estimates.estimates[i](d, t);
                                if (prob > max_prob) {
                                    max_prob = prob;
                                    inferred_assignment = i;
                                }
                            }

                            int true_assignment = real_data(d, t);
                            if (inferred_assignment == true_assignment) {
                                num_right_inferences++;
                            }
                            total_inferences++;
                        }
                    }

                    accuracy = (double) num_right_inferences /
                               (double) total_inferences;
                }
                else {
                    ConfusionMatrix confusion_matrix =
                        this->get_confusion_matrix(estimates, test_data);
                    accuracy = (confusion_matrix.true_positives +
                                confusion_matrix.true_negatives) /
                               (double)confusion_matrix.get_total();
                }

                evaluation.evaluation =
                    Eigen::MatrixXd::Constant(1, 1, accuracy);
                evaluations.push_back(evaluation);
            }

            return evaluations;
        }

        void Accuracy::get_info(nlohmann::json& json) const {
            json["name"] = "accuracy";
            json["threshold"] = this->threshold;
            this->estimator->get_info(json["estimator"]);
        }

    } // namespace model
} // namespace tomcat

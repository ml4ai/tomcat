#include "Accuracy.h"

namespace tomcat {
    namespace model {

        using namespace std;

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        Accuracy::Accuracy(shared_ptr<Estimator> estimator, double threshold)
            : Measure(estimator, threshold) {}

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
                    int num_right_inferences = 0;
                    int cols = estimates.estimates[0].cols();

                    for (int d = 0; d < test_data.get_num_data_points(); d++) {
                        vector<int> counts_per_assignment(
                            estimates.estimates.size());

                        for (int t = 0; t < cols; t++) {
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

                            counts_per_assignment[inferred_assignment]++;
                        }

                        int most_inferred_assignment = static_cast<int>(
                            distance(counts_per_assignment.begin(),
                                     max_element(counts_per_assignment.begin(),
                                                 counts_per_assignment.end())));
                        int true_assignment =
                            test_data[estimates.label].at(0, d, 0);
                        if (most_inferred_assignment == true_assignment) {
                            num_right_inferences++;
                        }
                    }

                    accuracy = (double) num_right_inferences /
                               (double)(test_data.get_num_data_points());
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

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

            for (const auto& estimates :
                 this->estimator->get_estimates()) {
                NodeEvaluation evaluation;
                evaluation.label = estimates.label;
                evaluation.assignment = estimates.assignment;

                ConfusionMatrix confusion_matrix =
                    this->get_confusion_matrix(estimates, test_data);
                double accuracy = (confusion_matrix.true_positives +
                                   confusion_matrix.true_negatives) /
                                  (double)confusion_matrix.get_total();

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

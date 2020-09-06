#include "EstimationProcess.h"

namespace tomcat {
    namespace model {

        using namespace std;

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        EstimationProcess::EstimationProcess() {}

        EstimationProcess::~EstimationProcess() {}

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void EstimationProcess::set_training_data(
            const tomcat::model::EvidenceSet& training_data) {
            for (auto& estimator : this->estimators) {
                estimator->set_training_data(training_data);
            }
        }

        void
        EstimationProcess::add_estimator(shared_ptr<Estimator> estimator) {
            this->estimators.push_back(estimator);
        }

        void EstimationProcess::reset() {
            for(auto& estimator :this->estimators){
                estimator->prepare();
            }
        }

        void EstimationProcess::copy_estimation(
            const EstimationProcess& estimation) {
            this->estimators = estimation.estimators;
        }

    } // namespace model
} // namespace tomcat

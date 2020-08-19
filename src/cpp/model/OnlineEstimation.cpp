#include "OnlineEstimation.h"

namespace tomcat {
    namespace model {

        //----------------------------------------------------------------------
        // Definitions
        //----------------------------------------------------------------------

        // No definitions in this file

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        OnlineEstimation::OnlineEstimation(
            std::shared_ptr<ModelEstimator> estimator,
            MessageBrokerConfiguration& config)
            : ModelEstimation(estimator), config(config) {}

        OnlineEstimation::OnlineEstimation(
            std::shared_ptr<ModelEstimator> estimator,
            MessageBrokerConfiguration&& config)
            : ModelEstimation(estimator), config(std::move(config)) {}

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
        void
        OnlineEstimation::copy_estimation(const OnlineEstimation& estimation) {
            ModelEstimation::copy_estimation(estimation);
            this->config = estimation.config;
        }

        void OnlineEstimation::estimate(EvidenceSet test_data) {
            // TODO - starts news connection with the message broker and listens
            //  to it forever (it's killed externally).
//            for(int t = 0; t < test_data.get_time_steps(); t++){
//                this->estimator->estimate(test_data.slice({t}, 2));
//            }
        }

    } // namespace model
} // namespace tomcat

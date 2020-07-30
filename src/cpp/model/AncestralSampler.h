#pragma once

#include "Sampler.h"

namespace tomcat {
    namespace model {

        /**
         * Sampler based on ancestral sampling process. When data is provided
         * for specific nodes, samples are not generated for these nodes and the
         * data provided will be considered when generating samples of their
         * child nodes.
         */
        class AncestralSampler : public Sampler {

          private:

            /**
             * Assign the values provided in the data to the corresponding
             * nodes.
             *
             * @param datapoint_index: index of the datapoint in the data matrix
             */
            void
            assign_data_to_node(const std::shared_ptr<RandomVariableNode>& node,
                                int datapoint_index);

            /**
             * Retrieve the row of the node's CPD table corresponding to its
             * parents' assignments, i.e. the index of the the distribution of
             * the node given it's parents' assignments.
             *
             * @param node: random variable node
             * @return Index of the node's distribution given its parents'
             * assignments
             */
            int get_distribution_index_given_parents_for(
                const RandomVariableNode& node) const;

          public:
            AncestralSampler(DynamicBayesNet model,
                             std::shared_ptr<gsl_rng> random_generator)
                : Sampler(model, random_generator) {}
            ~AncestralSampler() {}

            AncestralSampler(const AncestralSampler& sampler) {
                this->model = sampler.model;
            }
            AncestralSampler& operator=(const AncestralSampler& sampler) {
                this->model = sampler.model;
                return *this;
            }
            AncestralSampler(AncestralSampler&&) = default;
            AncestralSampler& operator=(AncestralSampler&&) = default;

            void sample(int num_samples, int time_steps) override;

            std::unique_ptr<Sampler> clone() override;
        };
    } // namespace model
} // namespace tomcat
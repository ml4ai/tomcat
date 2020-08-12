#pragma once

#include <unordered_set>

#include "DynamicBayesNet.h"
#include "Tensor3.h"

namespace tomcat {
    namespace model {

        /**
         * Abstract representation of a class to generate samples from a DBN
         * model.
         */
        class Sampler {
          public:
            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------
            /**
             * Creates an instance of an abstract sampler.
             */
            Sampler();

            /**
             * Creates an abstract instance of an abstract sampler for an
             * unrolled DBN.
             *
             * @param model: unrolled DBN
             * @param random_generator: random number generator
             */
            Sampler(DynamicBayesNet model,
                    std::shared_ptr<gsl_rng> random_generator);

            virtual ~Sampler();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------

            // Copy constructor and assignment should be deleted to avoid
            // implicit slicing and loss of polymorphic behaviour in the
            // subclasses. To deep copy, the clone method must be used.
            Sampler(const Sampler&) = delete;

            Sampler& operator=(const Sampler&) = delete;

            Sampler(Sampler&&) = default;

            Sampler& operator=(Sampler&&) = default;

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Generates samples for the latent nodes.
             *
             * @param num_samples: number of samples to generate
             */
            void sample(int num_samples);

            /**
             * Adds data to node of arbitrary sample size.
             *
             * @param node_label: node's label
             * @param data: observed values for the node over time. Data should
             * be a tensor of dimensions (sample_size, num_data_points,
             * time_steps)
             */
            void add_data(const std::string& node_label, Tensor3& data);

            /**
             * Returns samples generated for a specific latent node.
             *
             * @param node_label: latent node label
             * @return Samples over time. A matrix of dimension (num_samples,
             * time_steps).
             */
            Tensor3 get_samples(const std::string& node_label) const;

            /**
             * Saves generated samples to files in a specific folder.
             * @param output_folder: folder where the files should be saved.
             */
            void save_samples_to_folder(const std::string& output_folder) const;

            //------------------------------------------------------------------
            // Pure virtual functions
            //------------------------------------------------------------------

            /**
             * Function called by the function sample to generate samples for
             * unfrozen nodes. The function sample freezes observable nodes and
             * releases them in the end. Calling this function in between.
             *
             * @param num_samples: number of samples to generate
             */
            virtual void sample_latent(int num_samples) = 0;

          protected:
            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------
            std::shared_ptr<gsl_rng> random_generator;
            DynamicBayesNet model;

            // Labels of the nodes that were sampled (this has to be filled in
            // the derived classes).
            std::unordered_set<std::string> sampled_node_labels;

            // If provided for some node, the number of data points. It's a
            // single variable because it must be the same for all observable
            // nodes.
            int num_data_points = 0;

          private:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Freeze nodes that have data assigned to them.
             */
            void freeze_observable_nodes();

            /**
             * Unfreeze nodes that have data assigned to them.
             */
            void unfreeze_observable_nodes();

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------
            std::unordered_set<std::string> observable_node_labels;
        };
    } // namespace model
} // namespace tomcat
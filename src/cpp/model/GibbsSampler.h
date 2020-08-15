#pragma once

#include "Sampler.h"

namespace tomcat {
    namespace model {

        //------------------------------------------------------------------
        // Forward declarations
        //------------------------------------------------------------------

        //------------------------------------------------------------------
        // Structs
        //------------------------------------------------------------------

        /**
         * Class description here
         */
        class GibbsSampler : public Sampler {
          public: // same pattern for protected and private in this order
            //------------------------------------------------------------------
            // Types, Enums & Constants
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates an instance of the sampler for a given model and random
             * number generator.
             *
             * @param model: DBN
             * @param random_generator: Random number generator
             */
            GibbsSampler(DynamicBayesNet model,
                         std::shared_ptr<gsl_rng> random_generator,
                         int burn_in_period);

            ~GibbsSampler();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------
            GibbsSampler(const GibbsSampler& sampler);

            GibbsSampler& operator=(const GibbsSampler& sampler);

            GibbsSampler(GibbsSampler&&) = default;

            GibbsSampler& operator=(GibbsSampler&&) = default;

            //------------------------------------------------------------------
            // Operator overload
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Static functions
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------
            void sample_latent(int num_samples) override;

            //------------------------------------------------------------------
            // Virtual functions
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Pure virtual functions
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Getters & Setters
            //------------------------------------------------------------------

          private:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------
            /**
             * Populates the nodes' assignments with initial values.
             */
            void fill_initial_samples();

            /**
             * Samples from the posterior distribution of a data node
             * (non-parameter latent node). The posterior for a data node x is
             * given by P(x|Pa(x))P(Ch1(x)|x)...P(Chk(x)|x), where Pa(x) is the
             * parents of x and Chi(x) is the i-th child of x. P(Chi(x)|x) is a
             * vector containing the pdf of the current value assigned to the
             * i-th child f x for each one of the possible values x can take.
             *
             * @param node: data node
             * @param discard: indicates whether the sample should be discarded
             * or stored
             */
            void sample_data_node(std::shared_ptr<Node> node,
                                  bool discard);

            /**
             * Samples from the posterior distribution of a parameter node. The
             * posterior for a parameter node is given by its prior adjusted by
             * some sufficient statistics. For instance, if a parameter node has
             * a Dirichlet distribution as prior, its posterior will be a
             * Dirichlet with the coefficients updated according to the sampled
             * values of the data nodes that depend on this parameter. As data
             * nodes are sampled from the roots to the leaves, their sampled
             * values are informed to the parameter nodes they depend on so they
             * can update its sufficient statistics so as to avoid looping over
             * data nodes when sampling a parameter node. It only works with
             * conjugate priors.
             *
             * @param node: parameter node
             * @param discard: indicates whether the sample should be discarded
             * or stored
             */
            void sample_parameter_node(std::shared_ptr<Node> node,
                                       bool discard);

            /**
             * Returns the weights for a given node given by the product of the
             * pdfs of all of its children's assignments given all possible
             * node's assignments. It can be a matrix because child nodes can
             * have multiple assignments.
             *
             * @param node: node to compute the weights for
             */
            Eigen::MatrixXd get_weights_for(const std::shared_ptr<Node>& node);

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------
            int burn_in_period = 0;
        };

    } // namespace model
} // namespace tomcat

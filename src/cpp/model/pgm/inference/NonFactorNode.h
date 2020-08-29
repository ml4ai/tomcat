#pragma once

#include "ExactInferenceNode.h"

#include "../../utils/Definitions.h"

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
        class NonFactorNode : public ExactInferenceNode {
          public:
            //------------------------------------------------------------------
            // Types, Enums & Constants
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------
            NonFactorNode(const std::string& label,
                          int cardinality);

            ~NonFactorNode();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------

            // Copy constructor and assignment should be deleted to avoid
            // implicit slicing and loss of polymorphic behaviour in the
            // subclasses. To deep copy, the clone method must be used.
            NonFactorNode(const NonFactorNode&) = delete;

            NonFactorNode& operator=(const NonFactorNode&) = delete;

            NonFactorNode(NonFactorNode&&) = default;

            NonFactorNode& operator=(NonFactorNode&&) = default;

            //------------------------------------------------------------------
            // Operator overload
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Static functions
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------
            /**
             * Returns the element-wise product of all the incoming messages
             * excluding the one that comes from the destiny node.
             *
             * @return Outward message
             */
            Eigen::MatrixXd
            get_outward_message_to(const NodeName& node_name, int source_time_slice) const override;

            Eigen::MatrixXd get_marginal_at(int time_step) const;

            void set_data_at(int time_step, const Eigen::VectorXd& data);

            //------------------------------------------------------------------
            // Virtual functions
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Pure virtual functions
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Getters & Setters
            //------------------------------------------------------------------
            int get_cardinality() const;

          protected:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------

          private:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------
            int cardinality;

            // Stores observable data for the node per time slice. The rows of
            // the data matrix contains a one-hot vector encode representing the
            // value observed for a particular data point. The number of rows in
            // the matrix is the number of data points observed.
            std::unordered_map<int, Eigen::MatrixXd> data_per_time_slice;
        };

    } // namespace model
} // namespace tomcat

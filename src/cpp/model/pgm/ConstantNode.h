#include "Node.h"

#include "utils/Definitions.h"

namespace tomcat {
    namespace model {

        /**
         * A node with constant numerical values assigned to it.
         */
        class ConstantNode : public Node {
          public:
            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates a constant node with a numerical value assigned to it.
             *
             * @param value: node's numerical assignment
             * @param label: node's label
             */
            ConstantNode(double value, std::string label = "unlabeled");

            /**
             * Creates a constant node with a multidimensional value assigned to it.
             *
             * @param values: node's constant assignment
             * * @param label: node's label
             */
            ConstantNode(const Eigen::VectorXd& values,
                         std::string label = "unlabeled");

            /**
             * Creates a constant node with a multidimensional value assigned to it.
             *
             * @param values: node's constant assignment
             * * @param label: node's label
             */
            ConstantNode(const Eigen::VectorXd&& values,
                         std::string label = "unlabeled");

            ~ConstantNode();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------
            ConstantNode(const ConstantNode& node);

            ConstantNode& operator=(const ConstantNode& node);

            ConstantNode(ConstantNode&&) = default;

            ConstantNode& operator=(ConstantNode&&) = default;

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------
            std::unique_ptr<Node> clone() const override;

            std::string get_description() const override;

            std::string get_timed_name() const override;

            //std::string get_timed_name(int time_step) const override;

          private:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Creates a default metadata for a constant node.
             *
             * @param label: node's label
             * @param sample_size: dimensionality of the value stored in the node.
             */
            void create_default_metadata(std::string& label, int sample_size);

        };

    } // namespace model
} // namespace tomcat

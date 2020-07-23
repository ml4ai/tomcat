#include "Node.h"

namespace tomcat {
    namespace model {

        /**
         * A node in a Dynamic Bayes Net (DBN).
         */
        class ConstantNode : public Node {
          public:
            /**
             * Create a constant node with a numerical value assigned.
             *
             * @param value: node's constant assignment
             */
            ConstantNode(double value, std::string label = "unlabeled");

            /**
             * Create a constant node with a multidimensional value assigned.
             *
             * @param values: node's constant assignment
             */
            ConstantNode(Eigen::VectorXd values, std::string label = "unlabeled") : Node(std::move(values)) {
                this->create_default_metadata(label);
            }
            ~ConstantNode() {}

            ConstantNode(const ConstantNode& node) {
                this->assignment = node.assignment;
            };
            ConstantNode& operator=(const ConstantNode& node) {
                this->assignment = node.assignment;
                return *this;
            };

            ConstantNode(ConstantNode&&) = default;
            ConstantNode& operator=(ConstantNode&&) = default;

            /**
             * Create default metadata for constant nodes.
             *
             * @param label: node's label
             */
            void create_default_metadata(std::string label);

            /**
             * Print a short description of the node.
             *
             * @param os: output stream
             */
            void print(std::ostream& os) const override;

            /**
             * Return a short description of the node.
             */
            std::string get_description() const override;

            /**
             * Clone node
             *
             * @return pointer to the new node
             */
            std::unique_ptr<Node> clone() const override;

            /**
             * Return the node label as a constant node is time agnostic.
             *
             * @return node label
             */
            std::string get_timed_name() const override;

            /**
             * Return the node label as a constant node is time agnostic.
             *
             * @return node label
             */
            std::string get_timed_name(int time_step) const override;

        };

    } // namespace model
} // namespace tomcat
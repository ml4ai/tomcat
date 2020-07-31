#include "Node.h"

namespace tomcat {
    namespace model {

        /**
         * A node in a Dynamic Bayes Net (DBN).
         */
        class ConstantNode : public Node {
          private:
            /**
             * Create default metadata for constant nodes.
             *
             * @param label: node's label
             * @param sample_size: dimensionality of the value stored in the node
             */
            void create_default_metadata(std::string& label, int sample_size);

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
            ConstantNode(Eigen::VectorXd& values,
                         std::string label = "unlabeled")
                : Node(values) {
                this->create_default_metadata(label, this->assignment.size());
            }

            ConstantNode(Eigen::VectorXd&& values,
                         std::string label = "unlabeled")
                : Node(std::move(values)) {
                this->create_default_metadata(label, this->assignment.size());
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

            void print(std::ostream& os) const override;

            std::string get_description() const override;

            std::unique_ptr<Node> clone() const override;

            std::string get_timed_name() const override;

            std::string get_timed_name(int time_step) const override;
        };

    } // namespace model
} // namespace tomcat
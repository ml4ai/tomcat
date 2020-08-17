#include "Node.h"

namespace tomcat {
    namespace model {

        //----------------------------------------------------------------------
        // Definitions
        //----------------------------------------------------------------------

        // No definitions in this file

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        Node::Node() {}

        Node::Node(std::shared_ptr<NodeMetadata> metadata)
            : metadata(std::move(metadata)) {}

        Node::~Node() {}

        //----------------------------------------------------------------------
        // Operator overload
        //----------------------------------------------------------------------
        std::ostream& operator<<(std::ostream& os, const Node& node) {
            node.print(os);
            return os;
        };

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void Node::print(std::ostream& os) const {
            os << this->get_description();
        }

        int Node::get_size() const {
            return this->assignment.rows();
        }

        //----------------------------------------------------------------------
        // Getters & Setters
        //----------------------------------------------------------------------
        const std::shared_ptr<NodeMetadata>& Node::get_metadata() const {
            return metadata;
        }

        const Eigen::MatrixXd& Node::get_assignment() const { return assignment; }

    } // namespace model
} // namespace tomcat

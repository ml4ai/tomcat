#include "Node.h"

namespace tomcat {
    namespace model {

        using namespace std;

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        Node::Node() {}

        Node::Node(shared_ptr<NodeMetadata> metadata)
            : metadata(move(metadata)) {}

        Node::~Node() {}

        //----------------------------------------------------------------------
        // Operator overload
        //----------------------------------------------------------------------
        ostream& operator<<(ostream& os, const Node& node) {
            node.print(os);
            return os;
        };

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void Node::print(ostream& os) const {
            os << this->get_description();
        }

        int Node::get_size() const {
            return this->assignment.rows();
        }

        //----------------------------------------------------------------------
        // Getters & Setters
        //----------------------------------------------------------------------
        const shared_ptr<NodeMetadata>& Node::get_metadata() const {
            return metadata;
        }

        const Eigen::MatrixXd& Node::get_assignment() const { return assignment; }

    } // namespace model
} // namespace tomcat

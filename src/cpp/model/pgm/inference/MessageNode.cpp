#include "MessageNode.h"

#include <sstream>

using namespace std;

namespace tomcat {
    namespace model {

        using namespace std;

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        MessageNode::MessageNode() {}

        MessageNode::MessageNode(const string& label, int time_step)
            : label(label), time_step(time_step) {}

        MessageNode::~MessageNode() {}

        //----------------------------------------------------------------------
        // Operator overload
        //----------------------------------------------------------------------
        bool MessageNode::operator==(const MessageNode& obj) const {
            return this->get_name() == obj.get_name();
        }

        //----------------------------------------------------------------------
        // Static functions
        //----------------------------------------------------------------------
        string MessageNode::get_name(const string& label,
                                          int time_step) {
            stringstream ss;
            ss << "(" << label << ", " << time_step << ")";
            return ss.str();
        }

        bool MessageNode::is_prior(const string& node_name) {
            return strip(node_name).first == PRIOR_NODE_LABEL;
        }

        bool MessageNode::is_end_node(const string& node_name) {
            return strip(node_name).first == END_NODE_LABEL;
        }

        pair<string, int> MessageNode::strip(const string& node_name) {
            size_t end_index = node_name.find(",");
            string label = node_name.substr(1, end_index - 1);
            int time_step =
                stoi(node_name.substr(end_index + 2, node_name.size() - 1));

            return make_pair(label, time_step);
        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        Eigen::MatrixXd
        MessageNode::get_incoming_message_from(const string& source_label,
                                               int source_time_step,
                                               int target_time_step) const {

            return this->incoming_messages_per_time_slice.at(target_time_step)
                .get_message_for(source_label, source_time_step);
        }

        void
        MessageNode::set_incoming_message_from(const string& source_label,
                                               int source_time_step,
                                               int target_time_step,
                                               const Eigen::MatrixXd& message) {

            this->max_time_step_stored =
                max(this->max_time_step_stored, target_time_step);

            this->incoming_messages_per_time_slice[target_time_step]
                .set_message_for(source_label, source_time_step, message);
        }

        string MessageNode::get_name() const {
            return get_name(this->label, this->time_step);
        }

        void MessageNode::copy_node(const MessageNode& node) {
            this->label = node.label;
            this->time_step = node.time_step;
            this->incoming_messages_per_time_slice =
                node.incoming_messages_per_time_slice;
        }

        void MessageNode::erase_incoming_messages_beyond(int time_step) {
            for (int t = time_step + 1; t <= this->max_time_step_stored; t++) {
                this->incoming_messages_per_time_slice.erase(t);
            }
            this->max_time_step_stored =
                min(this->max_time_step_stored, time_step);
        }

        //----------------------------------------------------------------------
        // Getters & Setters
        //----------------------------------------------------------------------
        const string& MessageNode::get_label() const { return label; }

        int MessageNode::get_time_step() const { return time_step; }

    } // namespace model
} // namespace tomcat

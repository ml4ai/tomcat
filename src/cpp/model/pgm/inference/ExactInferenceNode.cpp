#include "ExactInferenceNode.h"

#include <sstream>

using namespace std;

namespace tomcat {
    namespace model {

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        ExactInferenceNode::ExactInferenceNode(const std::string& label)
            : label(label) {}

        ExactInferenceNode::~ExactInferenceNode() {}

        //----------------------------------------------------------------------
        // Copy & Move constructors/assignments
        //----------------------------------------------------------------------
        //        InferenceNode(const ExactInferenceNode& InferenceNode) {}
        //
        //        ExactInferenceNode& operator=(const ExactInferenceNode&
        //        InferenceNode) {
        //        }

        //----------------------------------------------------------------------
        // Operator overload
        //----------------------------------------------------------------------

        //----------------------------------------------------------------------
        // Static functions
        //----------------------------------------------------------------------
        string ExactInferenceNode::get_unique_label_for(
            const std::string& original_label, int time_step) {
            stringstream ss;
            ss << original_label << time_step;
            return ss.str();
        }

        //        string ExactInferenceNode::get_node_name(const string&
        //        node_label,
        //                                                 int time_step) {
        //            stringstream ss;
        //            ss << "(" << node_label << ", " << time_step << ")";
        //            return ss.str();
        //        }
        //
        //        string ExactInferenceNode::get_node_label_from(const
        //        std::string& node_name) {
        //            unsigned first_index = node_name.find("(");
        //            unsigned last_index = node_name.find(",");
        //            return node_name.substr(first_index,last_index -
        //            first_index);
        //        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        //        string ExactInferenceNode::get_name() const {
        //            return ExactInferenceNode::get_node_name(this->label,
        //                                                     this->relative_time_step);
        //        }

        const Eigen::MatrixXd& ExactInferenceNode::get_incoming_message_from(
            const NodeName& node_name, int target_time_step) const {

            return this->incoming_messages_per_time_slice.at(target_time_step)
                .at(node_name);
        }

        void ExactInferenceNode::set_incoming_message_from(
            const NodeName& node_name,
            int target_time_step,
            const Eigen::MatrixXd& message) {

            this->incoming_messages_per_time_slice[target_time_step]
                                                  [node_name] = message;
        }

//        string ExactInferenceNode::get_original_label() const {
//            string original_label = label.substr(0, label.size() - 1);
//            return original_label;
//        }

        //----------------------------------------------------------------------
        // Getters & Setters
        //----------------------------------------------------------------------
        const string& ExactInferenceNode::get_label() const { return label; }

    } // namespace model
} // namespace tomcat

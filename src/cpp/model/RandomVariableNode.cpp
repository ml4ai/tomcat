#include "RandomVariableNode.h"
#include <algorithm>
#include <fmt/format.h>
#include <iterator>
#include <sstream>
#include <stdexcept>

namespace tomcat {
    namespace model {

#define exists(member, container) container.find(member) != container.end()

        void
        RandomVariableNode::copy_from_node(const RandomVariableNode& node) {
            this->metadata = node.metadata;
            this->cpds = node.cpds;
            this->time_step = node.time_step;
            this->assignment = node.assignment;
        }

        void RandomVariableNode::print(std::ostream& os) const {
            os << this->get_description();
        }

        std::string RandomVariableNode::get_description() const {

            if (this->assignment.size() == 1) {
                std::stringstream assignment_string;
                assignment_string << this->assignment;

                return fmt::format("RV({}, {}, {})",
                                   this->metadata->get_label(),
                                   this->time_step,
                                   assignment_string.str());
            }
            else {
                std::stringstream assignment_string;
                assignment_string << this->assignment.transpose();

                return fmt::format("RV({}, {}, [{}])",
                                   this->metadata->get_label(),
                                   this->time_step,
                                   assignment_string.str());
            }
        }

        void RandomVariableNode::set_assignment(double assignment) {
            if (this->assignment.size() == 1) {
                this->assignment(0) = assignment;
            }
            else {
                throw std::invalid_argument("The RV is not 1-dimensional.");
            }
        }

        std::unique_ptr<Node> RandomVariableNode::clone() const {
            return std::make_unique<RandomVariableNode>(*this);
        }

        std::string RandomVariableNode::get_timed_name() const {
            return this->get_timed_name(this->time_step);
        }

        std::string RandomVariableNode::get_timed_name(int time_step) const {
            return fmt::format("({},{})", metadata->get_label(), time_step);
        }

        static std::string
        get_unique_key_from_labels(std::vector<std::string> labels) {
            std::stringstream sstream;
            std::sort(labels.begin(), labels.end());
            copy(labels.begin(),
                 labels.end(),
                 std::ostream_iterator<std::string>(sstream, ","));
            return sstream.str();
        }

        void RandomVariableNode::add_cpd(std::shared_ptr<CPD>& cpd) {
            std::string key =
                get_unique_key_from_labels(cpd->get_parent_node_label_order());
            this->cpds[key] = cpd;
        }

        void RandomVariableNode::add_cpd(std::shared_ptr<CPD>&& cpd) {
            std::string key =
                get_unique_key_from_labels(cpd->get_parent_node_label_order());
            this->cpds[key] = std::move(cpd);
        }

        std::shared_ptr<CPD> RandomVariableNode::get_cpd_for(
            const std::vector<std::string>& parent_labels) const {
            std::string key = get_unique_key_from_labels(parent_labels);
            std::shared_ptr<CPD> cpd;
            if (exists(key, this->cpds)) {
                cpd = this->cpds.at(key);
            }
            else {
                throw std::invalid_argument(
                    "No CPD found associated with the parents informed.");
            }

            return cpd;
        }

        //        void RandomVariableNode::replace_cpd(std::shared_ptr<CPD>&
        //        cpd) {
        //            std::string key =
        //                get_unique_key_from_labels(cpd->get_parent_node_label_order());
        //            if (exists(key, this->cpds)) {
        //                this->cpds[key] = cpd;
        //            }
        //            else {
        //                throw std::invalid_argument(
        //                    "No equivalent CPD to be replaced.");
        //            }
        //        }

        void RandomVariableNode::update_cpd_dependencies(
            NodeMap& parameter_nodes_map, int time_step) {
            for (const auto& mapping : cpds) {
                if (!mapping.second->is_updated()) {
                    mapping.second->update_dependencies(parameter_nodes_map,
                                                        time_step);
                }
            }
        }

//        void RandomVariableNode::replace_cpd(std::shared_ptr<CPD>&& cpd) {
//            std::string key =
//                get_unique_key_from_labels(cpd->get_parent_node_label_order());
//            if (exists(key, this->cpds)) {
//                this->cpds[key] = std::move(cpd);
//            }
//            else {
//                throw std::invalid_argument(
//                    "No equivalent CPD to be replaced.");
//            }
//        }

        void RandomVariableNode::reset_cpds_updated_status() {
            for (const auto& mapping : this->cpds) {
                mapping.second->reset_updated();
            }
        }

        void RandomVariableNode::clone_cpds() {
            for (auto& mapping : this->cpds) {
                mapping.second = mapping.second->clone_shared();
            }
        }

    } // namespace model
} // namespace tomcat
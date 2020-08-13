#include "RandomVariableNode.h"
#include <algorithm>
#include <fmt/format.h>
#include <iterator>
#include <sstream>
#include <stdexcept>

namespace tomcat {
    namespace model {

        //----------------------------------------------------------------------
        // Definitions
        //----------------------------------------------------------------------
#define exists(member, container) container.find(member) != container.end()

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        RandomVariableNode::RandomVariableNode(
            std::shared_ptr<NodeMetadata>& metadata, int time_step)
            : Node(metadata), time_step(time_step) {}

        RandomVariableNode::RandomVariableNode(
            std::shared_ptr<NodeMetadata>&& metadata, int time_step)
            : Node(std::move(metadata)), time_step(time_step) {}

        RandomVariableNode::~RandomVariableNode() {}

        //----------------------------------------------------------------------
        // Copy & Move constructors/assignments
        //----------------------------------------------------------------------
        RandomVariableNode::RandomVariableNode(const RandomVariableNode& node) {
            this->copy_node(node);
        }

        RandomVariableNode&
        RandomVariableNode::operator=(const RandomVariableNode& node) {
            this->copy_node(node);
            return *this;
        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void
        RandomVariableNode::copy_node(const RandomVariableNode& node) {
            this->metadata = node.metadata;
            this->cpds = node.cpds;
            this->time_step = node.time_step;
            this->assignment = node.assignment;
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

        std::unique_ptr<Node> RandomVariableNode::clone() const {
            std::unique_ptr<RandomVariableNode> new_node =
                std::make_unique<RandomVariableNode>(*this);
            new_node->metadata =
                std::make_shared<NodeMetadata>(*this->metadata);
            new_node->clone_cpds();
            return new_node;
        }

        std::string RandomVariableNode::get_timed_name() const {
            return this->metadata->get_timed_name(this->time_step);
        }

        void RandomVariableNode::add_cpd(std::shared_ptr<CPD>& cpd) {
            this->cpds[cpd->get_id()] = cpd;
        }

        void RandomVariableNode::add_cpd(std::shared_ptr<CPD>&& cpd) {
            this->cpds[cpd->get_id()] = std::move(cpd);
        }

        std::shared_ptr<CPD> RandomVariableNode::get_cpd_for(
            const std::vector<std::string>& parent_labels) const {
            std::string key = this->get_unique_key_from_labels(parent_labels);
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

        std::string RandomVariableNode::get_unique_key_from_labels(
            std::vector<std::string> labels) const {
            std::stringstream ss;
            std::sort(labels.begin(), labels.end());
            copy(labels.begin(),
                 labels.end(),
                 std::ostream_iterator<std::string>(ss, ","));
            return ss.str();
        }

        void RandomVariableNode::reset_cpds_updated_status() {
            for (const auto& mapping : this->cpds) {
                mapping.second->reset_updated_status();
            }
        }

        void RandomVariableNode::update_cpd_dependencies(
            NodeMap& parameter_nodes_map, int time_step) {
            for (const auto& mapping : cpds) {
                if (!mapping.second->is_updated()) {
                    mapping.second->update_dependencies(parameter_nodes_map,
                                                        time_step);
                }
            }
        }

        void RandomVariableNode::clone_cpds() {
            for (auto& mapping : this->cpds) {
                mapping.second = mapping.second->clone();
            }
        }

        Eigen::MatrixXd RandomVariableNode::sample(
            std::shared_ptr<gsl_rng> random_generator,
            const std::vector<std::shared_ptr<RandomVariableNode>>&
                parent_nodes,
            int num_samples) const {

            std::vector<std::string> parent_labels;
            parent_labels.reserve(parent_nodes.size());

            // This mapping will make it easy to access the parent node's object
            // by it's label
            Node::NodeMap parent_labels_to_nodes;
            for (const auto& parent_node : parent_nodes) {
                std::string label = parent_node->get_metadata()->get_label();
                parent_labels.push_back(label);
                // Moving the reference because there's no need to keep it in
                // the parent_nodes vector beyond this point
                parent_labels_to_nodes[label] = std::move(parent_node);
            }

            std::shared_ptr<CPD> cpd = this->get_cpd_for(parent_labels);

            return cpd->sample(
                random_generator, parent_labels_to_nodes, num_samples);
        }

        void RandomVariableNode::freeze() { RandomVariableNode::frozen = true; }

        void RandomVariableNode::unfreeze() {
            RandomVariableNode::frozen = false;
        }

        // ---------------------------------------------------------------------
        // Getters & Setters
        // ---------------------------------------------------------------------
        int RandomVariableNode::get_time_step() const { return time_step; }

        void RandomVariableNode::set_time_step(int time_step) {
            this->time_step = time_step;
        }

        void RandomVariableNode::set_assignment(Eigen::MatrixXd assignment) {
            if (!this->frozen) {
                this->assignment = assignment;
            }
        }

        bool RandomVariableNode::is_frozen() const { return frozen; }

    } // namespace model
} // namespace tomcat
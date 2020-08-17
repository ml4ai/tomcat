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
        void RandomVariableNode::copy_node(const RandomVariableNode& node) {
            this->metadata = node.metadata;
            this->time_step = node.time_step;
            this->assignment = node.assignment;
            this->cpd_templates = node.cpd_templates;
            this->cpd = node.cpd;
            this->frozen = node.frozen;
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
            new_node->clone_cpd_templates();
            new_node->clone_cpd();
            return new_node;
        }

        void RandomVariableNode::clone_cpd_templates() {
            for (auto& mapping : this->cpd_templates) {
                mapping.second = mapping.second->clone();
            }
        }

        void RandomVariableNode::clone_cpd() {
            if (this->cpd != nullptr) {
                this->cpd = this->cpd->clone();
            }
        }

        std::string RandomVariableNode::get_timed_name() const {
            return this->metadata->get_timed_name(this->time_step);
        }

        void RandomVariableNode::reset_cpd_updated_status() {
            this->cpd->reset_updated_status();
        }

        void RandomVariableNode::update_cpd_templates_dependencies(
            NodeMap& parameter_nodes_map, int time_step) {
            for(auto& mapping : this->cpd_templates) {
                if (!mapping.second->is_updated()) {
                    mapping.second->update_dependencies(parameter_nodes_map,
                                                   time_step);
                }
            }
        }

        Eigen::MatrixXd RandomVariableNode::sample(
            std::shared_ptr<gsl_rng> random_generator,
            const std::vector<std::shared_ptr<Node>>& parent_nodes,
            int num_samples) const {

            return this->cpd->sample(
                random_generator, parent_nodes, num_samples);
        }

        Eigen::MatrixXd RandomVariableNode::sample(
            std::shared_ptr<gsl_rng> random_generator,
            const std::vector<std::shared_ptr<Node>>& parent_nodes,
            int num_samples,
            Eigen::MatrixXd weights) const {

            return this->cpd->sample(
                random_generator, parent_nodes, num_samples, weights);
        }

        Eigen::MatrixXd RandomVariableNode::sample_from_conjugacy(
            std::shared_ptr<gsl_rng> random_generator,
            const std::vector<std::shared_ptr<Node>>& parent_nodes,
            int num_samples) const {
            return this->cpd->sample_from_conjugacy(
                random_generator, parent_nodes, num_samples);
        }

        Eigen::VectorXd RandomVariableNode::get_pdfs(
            const std::vector<std::shared_ptr<Node>>& parent_nodes) const {

            return this->cpd->get_pdfs(parent_nodes, *this);
        }

        void RandomVariableNode::update_parents_sufficient_statistics(
            const std::vector<std::shared_ptr<Node>>& parent_nodes) {

            this->cpd->update_sufficient_statistics(parent_nodes,
                                                    this->assignment);
        }

        void RandomVariableNode::add_to_sufficient_statistics(
            const Eigen::VectorXd& sample) {
            this->cpd->add_to_sufficient_statistics(sample);
        }

        void RandomVariableNode::reset_sufficient_statistics() {
            this->cpd->reset_sufficient_statistics();
        }

        void RandomVariableNode::freeze() { RandomVariableNode::frozen = true; }

        void RandomVariableNode::unfreeze() {
            RandomVariableNode::frozen = false;
        }

        void RandomVariableNode::add_cpd_template(std::shared_ptr<CPD>& cpd) {
            this->cpd_templates[cpd->get_id()] = cpd;
        }

        void RandomVariableNode::add_cpd_template(std::shared_ptr<CPD>&& cpd) {
            this->cpd_templates[cpd->get_id()] = std::move(cpd);
        }

        std::shared_ptr<CPD> RandomVariableNode::get_cpd_for(
            const std::vector<std::string>& parent_labels) const {
            std::string key = this->get_unique_key_from_labels(parent_labels);
            std::shared_ptr<CPD> cpd;
            if (exists(key, this->cpd_templates)) {
                cpd = this->cpd_templates.at(key);
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

        void RandomVariableNode::set_cpd(const std::shared_ptr<CPD>& cpd) {
            this->cpd = cpd;
        }

    } // namespace model
} // namespace tomcat
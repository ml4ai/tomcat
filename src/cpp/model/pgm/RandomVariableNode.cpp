#include "RandomVariableNode.h"
#include <algorithm>
#include <fmt/format.h>
#include <iterator>
#include <stdexcept>

namespace tomcat {
    namespace model {

        using namespace std;

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        RandomVariableNode::RandomVariableNode(
            shared_ptr<NodeMetadata>& metadata, int time_step)
            : Node(metadata), time_step(time_step) {}

        RandomVariableNode::RandomVariableNode(
            shared_ptr<NodeMetadata>&& metadata, int time_step)
            : Node(move(metadata)), time_step(time_step) {}

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

        string RandomVariableNode::get_description() const {

            if (this->assignment.size() == 1) {
                stringstream assignment_string;
                assignment_string << this->assignment;

                return fmt::format("RV({}, {}, {})",
                                   this->metadata->get_label(),
                                   this->time_step,
                                   assignment_string.str());
            }
            else {
                stringstream assignment_string;
                assignment_string << this->assignment.transpose();

                return fmt::format("RV({}, {}, [{}])",
                                   this->metadata->get_label(),
                                   this->time_step,
                                   assignment_string.str());
            }
        }

        unique_ptr<Node> RandomVariableNode::clone() const {
            unique_ptr<RandomVariableNode> new_node =
                make_unique<RandomVariableNode>(*this);
            new_node->metadata = make_shared<NodeMetadata>(*this->metadata);
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

        string RandomVariableNode::get_timed_name() const {
            return this->metadata->get_timed_name(this->time_step);
        }

        void RandomVariableNode::reset_cpd_updated_status() {
            this->cpd->reset_updated_status();
        }

        void RandomVariableNode::update_cpd_templates_dependencies(
            NodeMap& parameter_nodes_map, int time_step) {
            for (auto& mapping : this->cpd_templates) {
                if (!mapping.second->is_updated()) {
                    mapping.second->update_dependencies(parameter_nodes_map,
                                                        time_step);
                }
            }
        }

        Eigen::MatrixXd
        RandomVariableNode::sample(shared_ptr<gsl_rng> random_generator,
                                   const vector<shared_ptr<Node>>& parent_nodes,
                                   int num_samples,
                                   bool equal_samples) const {

            return this->cpd->sample(
                random_generator, parent_nodes, num_samples, equal_samples);
        }

        Eigen::MatrixXd
        RandomVariableNode::sample(shared_ptr<gsl_rng> random_generator,
                                   const vector<shared_ptr<Node>>& parent_nodes,
                                   int num_samples,
                                   Eigen::MatrixXd weights,
                                   bool equal_samples) const {

            return this->cpd->sample(random_generator,
                                     parent_nodes,
                                     num_samples,
                                     weights,
                                     equal_samples);
        }

        Eigen::MatrixXd RandomVariableNode::sample_from_conjugacy(
            shared_ptr<gsl_rng> random_generator,
            const vector<shared_ptr<Node>>& parent_nodes,
            int num_samples) const {
            return this->cpd->sample_from_conjugacy(
                random_generator, parent_nodes, num_samples);
        }

        Eigen::VectorXd RandomVariableNode::get_pdfs(
            const vector<shared_ptr<Node>>& parent_nodes) const {

            return this->cpd->get_pdfs(parent_nodes, *this);
        }

        void RandomVariableNode::update_parents_sufficient_statistics(
            const vector<shared_ptr<Node>>& parent_nodes) {

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

        void RandomVariableNode::add_cpd_template(shared_ptr<CPD>& cpd) {
            this->cpd_templates[cpd->get_id()] = cpd;
        }

        void RandomVariableNode::add_cpd_template(shared_ptr<CPD>&& cpd) {
            this->cpd_templates[cpd->get_id()] = move(cpd);
        }

        shared_ptr<CPD> RandomVariableNode::get_cpd_for(
            const vector<string>& parent_labels) const {
            string key = this->get_unique_key_from_labels(parent_labels);
            shared_ptr<CPD> cpd;
            if (EXISTS(key, this->cpd_templates)) {
                cpd = this->cpd_templates.at(key);
            }
            else {
                throw invalid_argument(
                    "No CPD found associated with the parents informed.");
            }

            return cpd;
        }

        string RandomVariableNode::get_unique_key_from_labels(
            vector<string> labels) const {
            stringstream ss;
            sort(labels.begin(), labels.end());
            copy(labels.begin(),
                 labels.end(),
                 ostream_iterator<string>(ss, ","));
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

        void RandomVariableNode::set_cpd(const shared_ptr<CPD>& cpd) {
            this->cpd = cpd;
        }
        const shared_ptr<CPD>& RandomVariableNode::get_cpd() const {
            return cpd;
        }

    } // namespace model
} // namespace tomcat
#include "AncestralSampler.h"

namespace tomcat {
    namespace model {

#define exists(member, container) container.find(member) != container.end()

        void AncestralSampler::assign_data_to_node(
            const std::shared_ptr<RandomVariableNode>& node,
            int datapoint_index) {

            std::string node_label = node->get_metadata()->get_label();

            if (exists(node_label, this->node_to_data)) {
                // todo - extend this to multidimensional sample size
                double assignment = this->node_to_data[node_label](
                    datapoint_index, node->get_time_step());
                node->set_assignment(assignment);
            }
        }

        Eigen::VectorXd AncestralSampler::get_sample_given_parents_for(
            const RandomVariableNode& node) const {
            std::vector<std::shared_ptr<RandomVariableNode>> parent_nodes =
                this->model.get_parent_nodes_of(node, true);
            std::vector<std::string> parent_labels;
            parent_labels.reserve(parent_nodes.size());

            // Make it easy to access the parent node's content by it's label
            std::unordered_map<std::string, std::shared_ptr<RandomVariableNode>>
                labels_to_nodes;
            for (const auto& parent_node : parent_nodes) {
                // Moving the reference because there's no need to keep it in
                // the parent_nodes vector beyond this point
                std::string label = parent_node->get_metadata()->get_label();
                parent_labels.push_back(label);
                labels_to_nodes[label] = std::move(parent_node);
            }

            // Use the order defined in the node's CPD to calculate the index
            // based on the parent's assignment. This is given by using the
            // cardinalities and assignments.
            int least_significant_cum_cardinality = 1;
            int distribution_index = 0;
            std::shared_ptr<CPD> cpd = node.get_cpd_for(parent_labels);
            std::vector<std::string> sorted_parent_labels =
                cpd->get_parent_node_label_order();

            // Iterate in reverse order
            for (auto parent_label_ptr = sorted_parent_labels.rbegin();
                 parent_label_ptr != sorted_parent_labels.rend();
                 parent_label_ptr++) {

                // todo - it does not work with multidimensional sample size
                std::shared_ptr<RandomVariableNode> parent_node =
                    labels_to_nodes.at(*parent_label_ptr);
                int assignment =
                    static_cast<int>(parent_node->get_assignment()[0]);
                distribution_index +=
                    assignment * least_significant_cum_cardinality;
                least_significant_cum_cardinality *=
                    parent_node->get_metadata()->get_cardinality();
            }

            return cpd->sample(this->random_generator, distribution_index);
        }

        void AncestralSampler::sample(int num_samples, int time_steps) {
            this->model.unroll(time_steps, false);
            std::vector<std::shared_ptr<RandomVariableNode>> nodes =
                this->model.get_nodes_topological_order();

            this->check_data(num_samples);
            this->init_samples_matrix(num_samples, time_steps);

            for (int s = 0; s < num_samples; s++) {
                for (const auto& node : nodes) {
                    if (exists(node->get_metadata()->get_label(),
                               this->latent_node_labels)) {

                        Eigen::VectorXd assignment =
                            this->get_sample_given_parents_for(*node);

                        node->set_assignment(assignment);

                        // todo - fix for multidimensional sample size (e.g.
                        //  samples from a parameter with dirichlet prior)
                        this->node_to_samples.at(
                            node->get_metadata()->get_label())(
                            s, node->get_time_step()) =

                            static_cast<int>(assignment(0));
                    }
                    else {
                        this->assign_data_to_node(node, s);
                    }
                }
            }
        }

        std::unique_ptr<Sampler> AncestralSampler::clone() {
            return std::make_unique<AncestralSampler>(*this);
        }

    } // namespace model
} // namespace tomcat

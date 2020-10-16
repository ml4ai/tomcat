#include "Sampler.h"

#include <array>

#include <boost/filesystem.hpp>
#include <fmt/format.h>

#include "utils/FileHandler.h"

namespace tomcat {
    namespace model {

        using namespace std;

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        Sampler::Sampler() {}

        Sampler::Sampler(shared_ptr<DynamicBayesNet> model) : model(model) {}

        Sampler::~Sampler() {}

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void Sampler::copy_sampler(const Sampler& sampler) {
            this->model = sampler.model;
            this->num_in_plate_samples = sampler.num_in_plate_samples;
            this->sampled_node_labels = sampler.sampled_node_labels;
        }

        void Sampler::sample(shared_ptr<gsl_rng> random_generator,
                             int num_samples) {
            this->freeze_observable_nodes();
            this->sample_latent(random_generator, num_samples);
            this->unfreeze_observable_nodes();
        }

        void Sampler::add_data(const EvidenceSet& data) {
            if (data.get_num_data_points() == this->num_in_plate_samples) {
                this->data = data;
                for (const auto& node_label : this->data.get_node_labels()) {
                    for (auto& node :
                         this->model->get_nodes_by_label(node_label)) {
                        shared_ptr<RandomVariableNode> rv_node =
                            dynamic_pointer_cast<RandomVariableNode>(node);

                        Tensor3 node_data = data[node_label];
                        rv_node->set_assignment(
                            node_data(rv_node->get_time_step(), 2).transpose());
                    }
                }
            }
            else {
                throw invalid_argument(
                    "The number of data points must be equal to the number of "
                    "samples of in-plate nodes defined for the sampler.");
            }
        }

        Tensor3 Sampler::get_samples(const string& node_label) const {
            vector<shared_ptr<Node>> nodes =
                this->model->get_nodes_by_label(node_label);

            // Get the max time step for which samples were generated
            int max_time_step = this->max_time_step_to_sample > 0
                                    ? min(this->max_time_step_to_sample,
                                          this->model->get_time_steps() - 1)
                                    : this->model->get_time_steps() - 1;

            if (!nodes.empty()) {
                int d1 = nodes[0]->get_metadata()->get_sample_size();
                int d2 = nodes[0]->get_size();
                int d3 = max_time_step + 1;
                double* buffer = new double[d1 * d2 * d3];
                fill_n(buffer, d1 * d2 * d3, NO_OBS);
                for (auto& node : nodes) {
                    shared_ptr<RandomVariableNode> rv_node =
                        dynamic_pointer_cast<RandomVariableNode>(node);
                    if (rv_node->get_time_step() <= max_time_step) {
                        Eigen::Matrix assignment = node->get_assignment();
                        for (int i = 0; i < assignment.rows(); i++) {
                            for (int j = 0; j < assignment.cols(); j++) {

                                // If a node is not replicable, it means there's
                                // only one instance of it in the unrolled DBN.
                                // This means there will be sample just for one
                                // time step, the one where the node shows up.
                                // However, the semantics of these nodes is that
                                // their assignments are applicable to all time
                                // steps starting from their initial one. So we
                                // replicate the samples to the remaining time
                                // steps until the maximum number of time steps
                                // sampled.
                                if (!node->get_metadata()->is_replicable()) {
                                    for (int t = rv_node->get_time_step();
                                         t <= max_time_step;
                                         t++) {
                                        int index = j * d2 * d3 + i * d3 + t;
                                        buffer[index] = assignment(i, j);
                                    }
                                }
                                else {
                                    int t = rv_node->get_time_step();
                                    int index = j * d2 * d3 + i * d3 + t;
                                    buffer[index] = assignment(i, j);
                                }
                            }
                        }
                    }
                }

                return Tensor3(buffer, d1, d2, d3);
            }
            else {
                throw invalid_argument(
                    "This node does not belong to the model.");
            }
        }

        void Sampler::save_samples_to_folder(
            const string& output_folder,
            const unordered_set<string> excluding) const {

            boost::filesystem::create_directories(output_folder);

            for (const auto& node_label : this->sampled_node_labels) {
                if (!EXISTS(node_label, excluding)) {
                    string filename = node_label + ".txt";
                    string filepath = get_filepath(output_folder, filename);
                    save_tensor_to_file(filepath,
                                        this->get_samples(node_label));
                }
            }
        }

        void Sampler::freeze_observable_nodes() {
            for (const auto& node_label : this->data.get_node_labels()) {
                for (auto& node : this->model->get_nodes_by_label(node_label)) {
                    dynamic_pointer_cast<RandomVariableNode>(node)->freeze();
                }
            }
        }

        void Sampler::unfreeze_observable_nodes() {
            for (const auto& node_label : this->data.get_node_labels()) {
                for (auto& node : this->model->get_nodes_by_label(node_label)) {
                    dynamic_pointer_cast<RandomVariableNode>(node)->unfreeze();
                }
            }
        }

        // ---------------------------------------------------------------------
        // Getters & Setters
        // ---------------------------------------------------------------------
        void Sampler::set_num_in_plate_samples(int num_in_plate_samples) {
            this->num_in_plate_samples = num_in_plate_samples;
        }

        const shared_ptr<DynamicBayesNet>& Sampler::get_model() const {
            return model;
        }

        void Sampler::set_max_time_step_to_sample(int maxTimeStepToSample) {
            this->max_time_step_to_sample = maxTimeStepToSample;
        }

    } // namespace model
} // namespace tomcat

#include "CPD.h"

namespace tomcat {
    namespace model {

        using namespace std;

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        CPD::CPD() {}

        CPD::CPD(vector<shared_ptr<NodeMetadata>>& parent_node_order)
            : parent_node_order(parent_node_order) {
            this->init_id();
            this->fill_indexing_mapping();
        }

        CPD::CPD(vector<shared_ptr<NodeMetadata>>&& parent_node_order)
            : parent_node_order(move(parent_node_order)) {
            this->init_id();
            this->fill_indexing_mapping();
        }

        CPD::CPD(vector<shared_ptr<NodeMetadata>>& parent_node_order,
                 vector<shared_ptr<Distribution>>& distributions)
            : parent_node_order(parent_node_order),
              distributions(distributions) {
            this->init_id();
            this->fill_indexing_mapping();
        }

        CPD::CPD(vector<shared_ptr<NodeMetadata>>&& parent_node_order,
                 vector<shared_ptr<Distribution>>&& distributions)
            : parent_node_order(move(parent_node_order)),
              distributions(move(distributions)) {
            this->init_id();
            this->fill_indexing_mapping();
        }

        CPD::~CPD() {}

        //----------------------------------------------------------------------
        // Operator overload
        //----------------------------------------------------------------------
        ostream& operator<<(ostream& os, const CPD& cpd) {
            cpd.print(os);
            return os;
        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void CPD::init_id() {
            vector<string> labels;
            labels.reserve(this->parent_node_order.size());

            for (const auto& metadata : this->parent_node_order) {
                labels.push_back(metadata->get_label());
            }

            sort(labels.begin(), labels.end());
            stringstream ss;
            copy(labels.begin(),
                 labels.end(),
                 ostream_iterator<string>(ss, ","));
            this->id = ss.str();
        }

        void CPD::fill_indexing_mapping() {
            int cum_cardinality = 1;

            for (int order = this->parent_node_order.size() - 1; order >= 0;
                 order--) {
                shared_ptr<NodeMetadata> metadata =
                    this->parent_node_order[order];

                ParentIndexing indexing(
                    order, metadata->get_cardinality(), cum_cardinality);
                this->parent_label_to_indexing[metadata->get_label()] =
                    indexing;

                cum_cardinality *= metadata->get_cardinality();
            }
        }

        void CPD::copy_cpd(const CPD& cpd) {
            this->id = cpd.id;
            this->updated = cpd.updated;
            this->parent_label_to_indexing = cpd.parent_label_to_indexing;
            this->parent_node_order = cpd.parent_node_order;
            this->distributions = cpd.distributions;
        }

        void CPD::update_dependencies(Node::NodeMap& parameter_nodes_map,
                                      int time_step) {

            for (auto& distribution : this->distributions) {
                distribution->update_dependencies(parameter_nodes_map,
                                                  time_step);
            }

            this->updated = true;
        }

        Eigen::MatrixXd
        CPD::sample(shared_ptr<gsl_rng> random_generator,
                    const vector<shared_ptr<Node>>& parent_nodes,
                    int num_samples,
                    bool equal_samples) const {

            vector<int> distribution_indices =
                this->get_distribution_indices(parent_nodes, num_samples);

            int sample_size = this->distributions[0]->get_sample_size();

            Eigen::MatrixXd samples(distribution_indices.size(), sample_size);
            int sample_index = 0;
            for (const auto& distribution_idx : distribution_indices) {
                if (distribution_idx == NO_OBS) {
                    // This happens if the mission ended sooner for a specific
                    // data point but not for the others. All the samples from
                    // nodes in time step greater than the end of the mission
                    // for that particular data point will be set to NO_OBS to
                    // avoid updating the sufficient statistics of the parameter
                    // nodes.
                    samples.row(sample_index) =
                        Eigen::VectorXd::Constant(1, 1, NO_OBS);
                }
                else {
                    Eigen::VectorXd assignment =
                        this->distributions[distribution_idx]->sample(
                            random_generator, sample_index);
                    samples.row(sample_index) = move(assignment);
                }
                sample_index++;

                if (equal_samples) {
                    // We generate one sample and replicate to the others. See
                    // below.
                    break;
                }
            }

            // Sample replication if requested. We use the distribution_indices
            // size because, even if the number of samples is greater than one,
            // if the node is a parent of an in-plate node, it always yields a
            // single sample. This information is encoded in the distribution
            // indices size.
            for (; sample_index < distribution_indices.size(); sample_index++) {
                samples.row(sample_index) = samples.row(0);
            }

            return samples;
        }

        vector<int> CPD::get_distribution_indices(
            const vector<shared_ptr<Node>>& parent_nodes,
            int num_samples) const {

            vector<int> indices(num_samples, 0);

            Node::NodeMap parent_labels_to_nodes =
                this->map_labels_to_nodes(parent_nodes);
            for (const auto& mapping : parent_labels_to_nodes) {
                string label = mapping.first;
                shared_ptr<Node> node = mapping.second;
                ParentIndexing indexing =
                    this->parent_label_to_indexing.at(label);

                Eigen::MatrixXd matrix = node->get_assignment();
                for (int i = 0; i < num_samples; i++) {
                    // Non-in-plate nodes will have a single assignment while
                    // in-plate nodes can have multiple assignments. The value
                    // of a non-in-plate node must be broadcasted.
                    int row = matrix.rows() == 1 ? 0 : i;
                    int parent_assignment_for_data_point =
                        static_cast<int>(matrix(row, 0));

                    if (parent_assignment_for_data_point == NO_OBS) {
                        // The mission ended sooner for this sample. Mark index
                        // of the distribution as NO_OBS so we don't use this
                        // sample to update the posterior of the parameters for
                        // time steps greater than the mission ending for this
                        // sample.
                        indices[i] = NO_OBS;
                    }
                    else {
                        // The formula to find the index if p1 * rcc_p1 + p2 *
                        // rcc_p2 ... where pi is the i-th parent of a node and
                        // rcc_p2 is the multiplication of the cardinalities of
                        // the parents pj, such that j > i.
                        indices[i] += parent_assignment_for_data_point *
                                      indexing.right_cumulative_cardinality;
                    }
                }
            }

            return indices;
        }

        Node::NodeMap
        CPD::map_labels_to_nodes(const vector<shared_ptr<Node>>& nodes) const {

            Node::NodeMap labels_to_nodes;
            for (auto& node : nodes) {
                string label = node->get_metadata()->get_label();
                labels_to_nodes[label] = node;
            }

            return labels_to_nodes;
        }

        Eigen::MatrixXd
        CPD::sample(shared_ptr<gsl_rng> random_generator,
                    const vector<shared_ptr<Node>>& parent_nodes,
                    int num_samples,
                    Eigen::MatrixXd weights,
                    bool equal_samples) const {

            vector<int> distribution_indices =
                this->get_distribution_indices(parent_nodes, num_samples);

            int sample_size = this->distributions[0]->get_sample_size();

            Eigen::MatrixXd samples(distribution_indices.size(), sample_size);
            int sample_index = 0;
            for (const auto& distribution_idx : distribution_indices) {
                Eigen::VectorXd data_point_weights = weights.row(sample_index);

                if (distribution_idx == NO_OBS ||
                    data_point_weights ==
                        Eigen::VectorXd::Constant(data_point_weights.size(),
                                                  NO_OBS)) {
                    // This happens if we stop observing values for a specific
                    // data point sooner than the others. We set the sample for
                    // NO_OBS to avoid updating the sufficient statistics of the
                    // parameter nodes given this nodes' assignment in that
                    // particular data point.
                    samples.row(sample_index) =
                        Eigen::VectorXd::Constant(1, 1, NO_OBS);
                }
                else {

                    Eigen::VectorXd assignment =
                        this->distributions[distribution_idx]->sample(
                            random_generator, sample_index, data_point_weights);
                    samples.row(sample_index) = move(assignment);
                }
                sample_index++;

                if (equal_samples) {
                    // We generate one sample and replicate to the others
                    break;
                }
            }

            // Sample replication if requested. We use the distribution_indices
            // size because, even if the number of samples is greater than one,
            // if the node is a parent of an in-plate node, it always yields a
            // single sample. This information is encoded in the distribution
            // indices size.
            for (; sample_index < distribution_indices.size(); sample_index++) {
                samples.row(sample_index) = samples.row(0);
            }

            return samples;
        }

        Eigen::VectorXd
        CPD::get_pdfs(const vector<shared_ptr<Node>>& parent_nodes,
                      const Node& node) const {

            vector<int> distribution_indices =
                this->get_distribution_indices(parent_nodes, node.get_size());

            Eigen::VectorXd pdfs(distribution_indices.size());
            int sample_index = 0;
            for (const auto& distribution_idx : distribution_indices) {
                Eigen::VectorXd data_point_node_assignment =
                    node.get_assignment().row(sample_index);

                if (distribution_idx == NO_OBS ||
                    data_point_node_assignment ==
                        Eigen::VectorXd::Constant(
                            data_point_node_assignment.size(), NO_OBS)) {
                    // There's no pdf for a node that was not observed in a
                    // given time step and data point.
                    pdfs(sample_index) = 0;
                }
                else {
                    shared_ptr<Distribution> distribution =
                        this->distributions[distribution_idx];
                    double pdf = distribution->get_pdf(
                        node.get_assignment().row(sample_index), sample_index);
                    pdfs(sample_index) = pdf;
                }
                sample_index++;
            }

            return pdfs;
        }

        void CPD::update_sufficient_statistics(
            const vector<shared_ptr<Node>>& parent_nodes,
            const Eigen::MatrixXd& cpd_owner_assignments) {

            vector<int> distribution_indices = this->get_distribution_indices(
                parent_nodes, cpd_owner_assignments.rows());

            int sample_index = 0;
            for (const auto& distribution_idx : distribution_indices) {
                // Do not include data points with no observation in the time
                // step being processed in the parameter nodes' sufficient
                // statistics update.
                Eigen::VectorXd assignment =
                    cpd_owner_assignments.row(sample_index);
                if (distribution_idx != NO_OBS &&
                    assignment !=
                        Eigen::VectorXd::Constant(assignment.size(), NO_OBS)) {
                    this->distributions[distribution_idx]
                        ->update_sufficient_statistics(assignment);
                } else {
                    LOG(assignment);
                }
                sample_index++;
            }
        }

        void CPD::reset_updated_status() { this->updated = false; }

        void CPD::print(ostream& os) const { os << this->get_description(); }

        Eigen::MatrixXd CPD::get_table() const {
            Eigen::MatrixXd table;

            int row = 0;
            for (const auto& distribution : this->distributions) {
                Eigen::VectorXd parameters = distribution->get_values();
                if (table.size() == 0) {
                    table = Eigen::MatrixXd(this->distributions.size(),
                                            parameters.size());
                    table.row(row) = parameters;
                }
                else {
                    table.row(row) = parameters;
                }
                row++;
            }

            return table;
        }

        //------------------------------------------------------------------
        // Getters & Setters
        //------------------------------------------------------------------
        const string& CPD::get_id() const { return id; }

        bool CPD::is_updated() const { return updated; }

        const CPD::TableOrderingMap& CPD::get_parent_label_to_indexing() const {
            return parent_label_to_indexing;
        }

    } // namespace model
} // namespace tomcat

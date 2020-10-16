#include "TomcatTA3V2.h"

#include "converter/TA3MessageConverter.h"
#include "pgm/NodeMetadata.h"
#include "pgm/RandomVariableNode.h"
#include "pgm/cpd/CategoricalCPD.h"
#include "pgm/cpd/DirichletCPD.h"

using namespace std;

namespace tomcat {
    namespace model {

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        TomcatTA3V2::TomcatTA3V2() {}

        TomcatTA3V2::~TomcatTA3V2() {}

        //----------------------------------------------------------------------
        // Copy & Move constructors/assignments
        //----------------------------------------------------------------------
        TomcatTA3V2::TomcatTA3V2(const TomcatTA3V2& tomcat) {
            this->copy_tomcat(tomcat);
        }

        TomcatTA3V2& TomcatTA3V2::operator=(const TomcatTA3V2& tomcat) {
            this->copy_tomcat(tomcat);
            return *this;
        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void TomcatTA3V2::init() {
            unordered_map<string, shared_ptr<RandomVariableNode>> nodes =
                this->get_nodes();

            // Connections
            int num_parameters =
                NUM_BEEP_STATES * NUM_TRAINING_CONDITIONS * NUM_STATES;
            for (int i = 0; i < num_parameters; i++) {
                stringstream parameter_label;
                parameter_label << THETA_S << i;
                shared_ptr<NodeMetadata> parameter_metadata =
                    nodes[parameter_label.str()]->get_metadata();
                nodes[STATE]->get_metadata()->add_parent_link(
                    parameter_metadata, true);
            }
            nodes[STATE]->get_metadata()->add_parent_link(
                nodes[STATE]->get_metadata(), true); // S_{t-1} -> S_t
            nodes[STATE]->get_metadata()->add_parent_link(
                nodes[TA3MessageConverter::Q]->get_metadata(),
                true); // Q -> S_t for all t
            nodes[STATE]->get_metadata()->add_parent_link(
                nodes[PBAE]->get_metadata(), true); // PBAE_{t-1} -> S_t
            nodes[TA3MessageConverter::ROOM]->get_metadata()->add_parent_link(
                nodes[STATE]->get_metadata(), false); // S_t -> Room_t
            nodes[TA3MessageConverter::SG]->get_metadata()->add_parent_link(
                nodes[STATE]->get_metadata(), false); // S_t -> Green_t
            nodes[TA3MessageConverter::SY]->get_metadata()->add_parent_link(
                nodes[STATE]->get_metadata(), false); // S_t -> Yellow_t
            nodes[TA3MessageConverter::BEEP]->get_metadata()->add_parent_link(
                nodes[PBAE]->get_metadata(), false); // Beep_t -> PBAE_t
            nodes[PBAE]->get_metadata()->add_parent_link(
                nodes[THETA_PBAE_PRIOR]->get_metadata(),
                false); // Theta_PBAE_Prior -> PBAE_{t}
            for (int i = 0; i < NUM_BEEP_STATES; i++) {
                stringstream parameter_label;
                parameter_label << THETA_PBAE << i;
                shared_ptr<NodeMetadata> parameter_metadata =
                    nodes[parameter_label.str()]->get_metadata();
                nodes[PBAE]->get_metadata()->add_parent_link(
                    parameter_metadata, true);
            }
            nodes[PBAE]->get_metadata()->add_parent_link(
                nodes[PBAE]->get_metadata(), true); // PBAE_{t-1} -> PBAE_t
            nodes[TA3MessageConverter::Q]->get_metadata()->add_parent_link(
                nodes[THETA_Q_PRIOR]->get_metadata(),
                false); // Theta_Q -> Q_{t} for all t

            DynamicBayesNet dbn;

            for (const auto& [node_label, node] : nodes) {
                dbn.add_node_template(*node);
            }

            this->model = make_shared<DynamicBayesNet>(dbn);
            this->model->unroll(NUM_SECONDS, true);
        }

        std::unordered_map<std::string, std::shared_ptr<RandomVariableNode>>
        TomcatTA3V2::get_nodes() const {
            // Nodes from v1.0
            std::unordered_map<std::string, std::shared_ptr<RandomVariableNode>>
                nodes = TomcatTA3::get_nodes();

            // New Theta_S nodes
            int num_parameters =
                NUM_BEEP_STATES * NUM_TRAINING_CONDITIONS * NUM_STATES;
            vector<shared_ptr<NodeMetadata>> theta_s_metadatas =
                create_theta_s_metadatas();
            vector<shared_ptr<CPD>> theta_s_cpds =
                this->create_theta_s_prior_cpds();
            vector<shared_ptr<RandomVariableNode>> theta_s_nodes(
                num_parameters);
            for (int i = 0; i < num_parameters; i++) {
                theta_s_nodes[i] =
                    this->create_node(theta_s_metadatas[i], {theta_s_cpds[i]});
                nodes[theta_s_metadatas[i]->get_label()] = theta_s_nodes[i];
            }

            // Theta_Q
            shared_ptr<NodeMetadata> theta_q_metadata =
                this->create_theta_q_prior_metadata();
            shared_ptr<CPD> theta_q_prior_cpd =
                this->create_theta_q_prior_cpd();
            nodes[THETA_Q_PRIOR] =
                this->create_node(theta_q_metadata, {theta_q_prior_cpd});

            // Theta_PBAE_Prior
            shared_ptr<NodeMetadata> theta_pbae_prior_metadata =
                this->create_theta_pbae_prior_metadata();
            shared_ptr<CPD> theta_pbae_prior_prior_cpd =
                this->create_theta_pbae_prior_prior_cpd();
            nodes[THETA_PBAE_PRIOR] = this->create_node(
                theta_pbae_prior_metadata, {theta_pbae_prior_prior_cpd});

            // Theta_PBAE
            vector<shared_ptr<NodeMetadata>> theta_pbae_metadatas =
                create_theta_pbae_metadatas();
            vector<shared_ptr<CPD>> theta_pbae_prior_cpds =
                this->create_theta_pbae_prior_cpds();
            vector<shared_ptr<RandomVariableNode>> theta_pbae_nodes(
                NUM_BEEP_STATES);
            for (int i = 0; i < NUM_BEEP_STATES; i++) {
                theta_pbae_nodes[i] = this->create_node(
                    theta_pbae_metadatas[i], {theta_pbae_prior_cpds[i]});
                nodes[theta_pbae_metadatas[i]->get_label()] =
                    theta_pbae_nodes[i];
            }

            // Training Condition
            shared_ptr<NodeMetadata> training_condition_metadata =
                this->create_training_condition_metadata();
            shared_ptr<CPD> training_condition_prior_cpd =
                this->create_training_condition_prior_cpd(nodes[THETA_Q_PRIOR]);
            nodes[TA3MessageConverter::Q] = this->create_node(
                training_condition_metadata, {training_condition_prior_cpd});

            // Player's Belief About the Environment
            shared_ptr<NodeMetadata> pbae_metadata =
                this->create_pbae_metadata();
            shared_ptr<CPD> pbae_prior_cpd =
                this->create_pbae_prior_cpd(nodes[THETA_PBAE_PRIOR]);
            shared_ptr<CPD> pbae_transition_cpd =
                this->create_state_transition_cpd({pbae_metadata},
                                                  theta_pbae_nodes);
            nodes[PBAE] = this->create_node(
                pbae_metadata, {pbae_prior_cpd, pbae_transition_cpd});

            // Beep
            shared_ptr<NodeMetadata> beep_metadata =
                this->create_beep_metadata();
            shared_ptr<CPD> beep_prior_cpd =
                this->create_beep_cpd(pbae_metadata);
            nodes[TA3MessageConverter::BEEP] =
                this->create_node(beep_metadata, {beep_prior_cpd});

            // New State with more parents
            shared_ptr<NodeMetadata> state_metadata =
                this->create_state_metadata();
            shared_ptr<CPD> state_prior_cpd = this->create_state_prior_cpd();
            shared_ptr<CPD> state_transition_cpd =
                this->create_state_transition_cpd({training_condition_metadata,
                                                   pbae_metadata,
                                                   state_metadata},
                                                  theta_s_nodes);
            nodes[STATE] = this->create_node(
                state_metadata, {state_prior_cpd, state_transition_cpd});

            return nodes;
        }

        vector<shared_ptr<NodeMetadata>>
        TomcatTA3V2::create_theta_s_metadatas() const {
            int num_parameters =
                NUM_BEEP_STATES * NUM_TRAINING_CONDITIONS * NUM_STATES;

            vector<shared_ptr<NodeMetadata>> theta_s_metadatas(num_parameters);
            for (int i = 0; i < num_parameters; i++) {
                stringstream parameter_label;
                parameter_label << THETA_S << i;

                NodeMetadata metadata =
                    NodeMetadata::create_multiple_time_link_metadata(
                        parameter_label.str(),
                        false,
                        true,
                        false,
                        1,
                        NUM_STATES);
                theta_s_metadatas[i] =
                    make_shared<NodeMetadata>(move(metadata));
            }

            return theta_s_metadatas;
        }

        vector<shared_ptr<CPD>> TomcatTA3V2::create_theta_s_prior_cpds() const {
            vector<shared_ptr<CPD>> cpds;

            for (int i = 0; i < NUM_BEEP_STATES * NUM_TRAINING_CONDITIONS;
                 i++) {
                vector<shared_ptr<CPD>> cpds_per_state =
                    TomcatTA3::create_theta_s_prior_cpds();
                cpds.insert(
                    cpds.end(), cpds_per_state.begin(), cpds_per_state.end());
            }

            return cpds;
        }

        shared_ptr<NodeMetadata>
        TomcatTA3V2::create_theta_q_prior_metadata() const {
            shared_ptr<NodeMetadata> metadata = make_shared<
                NodeMetadata>(NodeMetadata::create_single_time_link_metadata(
                THETA_Q_PRIOR, true, false, 1, NUM_TRAINING_CONDITIONS));

            return metadata;
        }

        shared_ptr<NodeMetadata>
        TomcatTA3V2::create_theta_pbae_prior_metadata() const {
            shared_ptr<NodeMetadata> metadata = make_shared<NodeMetadata>(
                NodeMetadata::create_single_time_link_metadata(
                    THETA_PBAE_PRIOR, true, false, 0, NUM_BEEP_STATES));

            return metadata;
        }

        vector<shared_ptr<NodeMetadata>>
        TomcatTA3V2::create_theta_pbae_metadatas() const {
            vector<shared_ptr<NodeMetadata>> theta_pbae_metadatas(
                NUM_BEEP_STATES);
            for (int i = 0; i < NUM_BEEP_STATES; i++) {
                stringstream parameter_label;
                parameter_label << THETA_PBAE << i;

                NodeMetadata metadata =
                    NodeMetadata::create_multiple_time_link_metadata(
                        parameter_label.str(),
                        false,
                        true,
                        false,
                        1,
                        NUM_BEEP_STATES);
                theta_pbae_metadatas[i] =
                    make_shared<NodeMetadata>(move(metadata));
            }

            return theta_pbae_metadatas;
        }

        shared_ptr<NodeMetadata>
        TomcatTA3V2::create_training_condition_metadata() const {
            shared_ptr<NodeMetadata> metadata = make_shared<NodeMetadata>(
                NodeMetadata::create_multiple_time_link_metadata(
                    TA3MessageConverter::Q,
                    false,
                    false,
                    true,
                    1,
                    1,
                    NUM_TRAINING_CONDITIONS));

            return metadata;
        }

        shared_ptr<NodeMetadata> TomcatTA3V2::create_pbae_metadata() const {
            shared_ptr<NodeMetadata> metadata = make_shared<NodeMetadata>(
                NodeMetadata::create_multiple_time_link_metadata(
                    PBAE, true, false, true, 0, 1, NUM_BEEP_STATES));

            return metadata;
        }

        shared_ptr<NodeMetadata> TomcatTA3V2::create_beep_metadata() const {
            shared_ptr<NodeMetadata> metadata = make_shared<NodeMetadata>(
                NodeMetadata::create_multiple_time_link_metadata(
                    TA3MessageConverter::BEEP,
                    true,
                    false,
                    true,
                    1,
                    1,
                    NUM_BEEP_STATES));

            return metadata;
        }

        shared_ptr<CPD> TomcatTA3V2::create_theta_q_prior_cpd() const {
            Eigen::MatrixXd cpd_table =
                Eigen::MatrixXd::Ones(1, NUM_TRAINING_CONDITIONS);
            DirichletCPD cpd_temp({}, cpd_table);
            shared_ptr<DirichletCPD> cpd =
                make_shared<DirichletCPD>(move(cpd_temp));

            return cpd;
        }

        shared_ptr<CPD> TomcatTA3V2::create_theta_pbae_prior_prior_cpd() const {
            Eigen::MatrixXd cpd_table =
                Eigen::MatrixXd::Ones(1, NUM_BEEP_STATES);
            DirichletCPD cpd_temp({}, cpd_table);
            shared_ptr<DirichletCPD> cpd =
                make_shared<DirichletCPD>(move(cpd_temp));

            return cpd;
        }

        vector<shared_ptr<CPD>>
        TomcatTA3V2::create_theta_pbae_prior_cpds() const {
            vector<shared_ptr<CPD>> cpds;
            cpds.reserve(NUM_BEEP_STATES);

            // All transitions possible
            DirichletCPD theta_pbae_cpd_temp(
                {}, Eigen::MatrixXd::Ones(1, NUM_BEEP_STATES));
            for (int i = 0; i < NUM_BEEP_STATES; i++) {
                cpds.push_back(make_shared<DirichletCPD>(theta_pbae_cpd_temp));
            }

            return cpds;
        }

        shared_ptr<CPD> TomcatTA3V2::create_training_condition_prior_cpd(
            std::shared_ptr<RandomVariableNode> theta_q_node) const {

            // This CPD table is not constant, it depends on the parameter node
            // Theta_Q.
            vector<shared_ptr<Categorical>> cpd_table = {
                make_shared<Categorical>(theta_q_node)};
            CategoricalCPD cpd_temp({}, cpd_table);
            shared_ptr<CategoricalCPD> cpd =
                make_shared<CategoricalCPD>(move(cpd_temp));

            return cpd;
        }

        shared_ptr<CPD> TomcatTA3V2::create_pbae_prior_cpd(
            std::shared_ptr<RandomVariableNode> theta_pbae_node) const {

            // This CPD table is not constant, it depends on the parameter node
            // Theta_Q.
            vector<shared_ptr<Categorical>> cpd_table = {
                make_shared<Categorical>(theta_pbae_node)};
            CategoricalCPD cpd_temp({}, cpd_table);
            shared_ptr<CategoricalCPD> cpd =
                make_shared<CategoricalCPD>(move(cpd_temp));

            return cpd;
        }

        shared_ptr<CPD> TomcatTA3V2::create_beep_cpd(
            std::shared_ptr<NodeMetadata> pbae_metadata) const {
            Eigen::MatrixXd cpd_table =
                Eigen::MatrixXd::Identity(NUM_BEEP_STATES, NUM_BEEP_STATES);
            CategoricalCPD cpd_temp({pbae_metadata}, cpd_table);
            shared_ptr<CategoricalCPD> cpd =
                make_shared<CategoricalCPD>(move(cpd_temp));

            return cpd;
        }

    } // namespace model
} // namespace tomcat

#include "TomcatTA3.h"

#include "pgm/NodeMetadata.h"
#include "pgm/RandomVariableNode.h"
#include "pgm/cpd/CategoricalCPD.h"
#include "pgm/cpd/DirichletCPD.h"

using namespace std;

namespace tomcat {
    namespace model {

        using namespace std;

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        TomcatTA3::TomcatTA3() {}

        TomcatTA3::~TomcatTA3() {}

        //----------------------------------------------------------------------
        // Copy & Move constructors/assignments
        //----------------------------------------------------------------------
        TomcatTA3::TomcatTA3(const TomcatTA3& tomcat) {
            this->copy_tomcat(tomcat);
        }

        TomcatTA3& TomcatTA3::operator=(const TomcatTA3& tomcat) {
            this->copy_tomcat(tomcat);
            return *this;
        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void TomcatTA3::init() {
            unordered_map<string, shared_ptr<RandomVariableNode>> nodes =
                this->get_nodes();

            // Connections
            for (int i = 0; i < NUM_STATES; i++) {
                stringstream parameter_label;
                parameter_label << THETA_S << i;
                shared_ptr<NodeMetadata> parameter_metadata =
                    nodes[parameter_label.str()]->get_metadata();
                nodes[STATE]->get_metadata()->add_parent_link(
                    parameter_metadata, true);
            }
            nodes[STATE]->get_metadata()->add_parent_link(
                nodes[STATE]->get_metadata(), true);
            nodes[ROOM]->get_metadata()->add_parent_link(
                nodes[STATE]->get_metadata(), false);
            nodes[SG]->get_metadata()->add_parent_link(
                nodes[STATE]->get_metadata(), false);
            nodes[SY]->get_metadata()->add_parent_link(
                nodes[STATE]->get_metadata(), false);

            DynamicBayesNet dbn;

            for (const auto& [node_label, node] : nodes) {
                dbn.add_node_template(*node);
            }

            this->model = make_shared<DynamicBayesNet>(dbn);
            this->model->unroll(NUM_SECONDS, true);
        }

        std::unordered_map<string, shared_ptr<RandomVariableNode>>
        TomcatTA3::get_nodes() const {
            unordered_map<string, shared_ptr<RandomVariableNode>> nodes;
            // Parameter nodes

            // Theta_S
            vector<shared_ptr<NodeMetadata>> theta_s_metadatas =
                create_theta_s_metadatas();
            vector<shared_ptr<CPD>> theta_s_cpds =
                this->create_theta_s_prior_cpds();
            vector<shared_ptr<RandomVariableNode>> theta_s_nodes(NUM_STATES);
            for (int i = 0; i < NUM_STATES; i++) {
                theta_s_nodes[i] = this->create_node(theta_s_metadatas[i], {theta_s_cpds[i]});
                nodes[theta_s_metadatas[i]->get_label()] = theta_s_nodes[i];
            }

            // Variables

            // State
            shared_ptr<NodeMetadata> state_metadata =
                this->create_state_metadata();
            shared_ptr<CPD> state_prior_cpd = this->create_state_prior_cpd();
            shared_ptr<CPD> state_transition_cpd =
                this->create_state_transition_cpd({state_metadata},
                                                  theta_s_nodes);
            nodes[STATE] = this->create_node(
                state_metadata, {state_prior_cpd, state_transition_cpd});

            // Room
            shared_ptr<NodeMetadata> room_metadata =
                this->create_room_metadata();
            shared_ptr<CPD> room_emission_cpd =
                this->create_room_cpd(state_metadata);
            nodes[ROOM] = this->create_node(room_metadata, {room_emission_cpd});

            // SG
            shared_ptr<NodeMetadata> sg_metadata = this->create_sg_metadata();
            shared_ptr<CPD> sg_emission_cpd =
                this->create_sg_cpd(state_metadata);
            nodes[SG] = this->create_node(sg_metadata, {sg_emission_cpd});

            // SY
            shared_ptr<NodeMetadata> sy_metadata = this->create_sy_metadata();
            shared_ptr<CPD> sy_emission_cpd =
                this->create_sy_cpd(state_metadata);
            nodes[SY] = this->create_node(sy_metadata, {sy_emission_cpd});

            return nodes;
        }

        vector<shared_ptr<NodeMetadata>> TomcatTA3::create_theta_s_metadatas() const {
            vector<shared_ptr<NodeMetadata>> theta_s_metadatas(NUM_STATES);
            for (int i = 0; i < NUM_STATES; i++) {
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

        shared_ptr<NodeMetadata> TomcatTA3::create_state_metadata() const {
            shared_ptr<NodeMetadata> metadata = make_shared<NodeMetadata>(
                NodeMetadata::create_multiple_time_link_metadata(
                    STATE, true, false, true, 0, 1, NUM_STATES));

            return metadata;
        }

        shared_ptr<NodeMetadata> TomcatTA3::create_room_metadata() const {
            shared_ptr<NodeMetadata> metadata = make_shared<NodeMetadata>(
                NodeMetadata::create_multiple_time_link_metadata(
                    ROOM, true, false, true, 1, 1, 2));

            return metadata;
        }

        shared_ptr<NodeMetadata> TomcatTA3::create_sg_metadata() const {
            shared_ptr<NodeMetadata> metadata = make_shared<NodeMetadata>(
                NodeMetadata::create_multiple_time_link_metadata(
                    SG, true, false, true, 1, 1, 2));

            return metadata;
        }

        shared_ptr<NodeMetadata> TomcatTA3::create_sy_metadata() const {
            shared_ptr<NodeMetadata> metadata = make_shared<NodeMetadata>(
                NodeMetadata::create_multiple_time_link_metadata(
                    SY, true, false, true, 1, 1, 2));

            return metadata;
        }

        vector<shared_ptr<CPD>> TomcatTA3::create_theta_s_prior_cpds() const {
            vector<shared_ptr<CPD>> cpds(NUM_STATES);

            // From HW to HW | RW | SG | SY
            Eigen::MatrixXd theta_s0(1, NUM_STATES);
            theta_s0 << 1, 1, EPSILON, EPSILON;
            DirichletCPD theta_s_cpd_temp({}, theta_s0);
            cpds[0] = make_shared<DirichletCPD>(move(theta_s_cpd_temp));

            // From RW to HW | RW | SG | SY
            Eigen::MatrixXd theta_s1(1, NUM_STATES);
            theta_s1 << 1, 1, 1, 1;
            theta_s_cpd_temp = DirichletCPD({}, theta_s1);
            cpds[1] = make_shared<DirichletCPD>(move(theta_s_cpd_temp));

            // From SG to HW | RW | SG | SY
            Eigen::MatrixXd theta_s2(1, NUM_STATES);
            theta_s2 << EPSILON, 1, 1, EPSILON;
            theta_s_cpd_temp = DirichletCPD({}, theta_s2);
            cpds[2] = make_shared<DirichletCPD>(move(theta_s_cpd_temp));

            // From SY to HW | RW | SG | SY
            Eigen::MatrixXd theta_s3(1, NUM_STATES);
            theta_s3 << EPSILON, 1, EPSILON, 1;
            theta_s_cpd_temp = DirichletCPD({}, theta_s3);
            cpds[3] = make_shared<DirichletCPD>(move(theta_s_cpd_temp));

            return cpds;
        }

        std::shared_ptr<CPD> TomcatTA3::create_state_prior_cpd() const {
            Eigen::MatrixXd cpd_table =
                Eigen::MatrixXd::Constant(1, NUM_STATES, EPSILON);
            cpd_table(0, 0) = 1; // The first state is always 0
            CategoricalCPD cpd_temp({}, cpd_table);
            shared_ptr<CategoricalCPD> cpd =
                make_shared<CategoricalCPD>(move(cpd_temp));

            return cpd;
        }

        std::shared_ptr<CPD> TomcatTA3::create_state_transition_cpd(
            std::vector<std::shared_ptr<NodeMetadata>> parent_nodes_metadata,
            std::vector<std::shared_ptr<RandomVariableNode>> theta_s_nodes) const {

            // This CPD table is not constant, it depends on parameter nodes.
            vector<shared_ptr<Categorical>> cpd_table;
            cpd_table.reserve(theta_s_nodes.size());
            for (auto& theta_s_node : theta_s_nodes) {
                cpd_table.push_back(make_shared<Categorical>(theta_s_node));
            }
            CategoricalCPD cpd_temp(parent_nodes_metadata, cpd_table);
            shared_ptr<CategoricalCPD> cpd =
                make_shared<CategoricalCPD>(move(cpd_temp));

            return cpd;
        }

        std::shared_ptr<CPD> TomcatTA3::create_room_cpd(
            std::shared_ptr<NodeMetadata> state_metadata) const {

            Eigen::MatrixXd cpd_table(NUM_STATES, 2);
            cpd_table << 1, EPSILON, EPSILON, 1, EPSILON, 1, EPSILON, 1;
            CategoricalCPD cpd_temp({state_metadata}, cpd_table);
            shared_ptr<CategoricalCPD> cpd =
                make_shared<CategoricalCPD>(move(cpd_temp));

            return cpd;
        }

        std::shared_ptr<CPD>
        TomcatTA3::create_sg_cpd(std::shared_ptr<NodeMetadata> state_metadata) const {
            Eigen::MatrixXd cpd_table(NUM_STATES, 2);
            cpd_table << 1, EPSILON, 1, EPSILON, EPSILON, 1, 1, EPSILON;
            CategoricalCPD cpd_temp({state_metadata}, cpd_table);
            shared_ptr<CategoricalCPD> cpd =
                make_shared<CategoricalCPD>(move(cpd_temp));

            return cpd;
        }

        std::shared_ptr<CPD> TomcatTA3::create_sy_cpd(
            std::shared_ptr<NodeMetadata> state_metadata) const {
            Eigen::MatrixXd cpd_table(NUM_STATES, 2);
            cpd_table << 1, EPSILON, 1, EPSILON, 1, EPSILON, EPSILON, 1;
            CategoricalCPD cpd_temp({state_metadata}, cpd_table);
            shared_ptr<CategoricalCPD> cpd =
                make_shared<CategoricalCPD>(move(cpd_temp));

            return cpd;
        }

    } // namespace model
} // namespace tomcat

#include "Tomcat.h"

#include <vector>

#include "model/pgm/NodeMetadata.h"
#include "model/pgm/RandomVariableNode.h"
#include "model/pgm/cpd/CategoricalCPD.h"
#include "model/pgm/cpd/DirichletCPD.h"
#include "model/sampling/AncestralSampler.h"

using namespace std;

namespace tomcat {
    namespace model {

        using namespace std;

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        Tomcat::Tomcat() {}

        Tomcat::~Tomcat() {}

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void Tomcat::init_ta3_learnable_model() {
            int num_states = 4;

            // 1. Parameter nodes

            // 1.1 States

            // 1.1.1 Metadata
            vector<shared_ptr<NodeMetadata>> theta_s_metadatas(num_states);
            for (int i = 0; i < num_states; i++) {
                stringstream parameter_label;
                parameter_label << THETA_S << i;

                NodeMetadata metadata =
                    NodeMetadata::create_multiple_time_link_metadata(
                        parameter_label.str(),
                        false,
                        true,
                        false,
                        1,
                        num_states);
                theta_s_metadatas[i] =
                    make_shared<NodeMetadata>(move(metadata));
            }

            // 1.1.2 CPD (priors of the parameters)
            vector<shared_ptr<DirichletCPD>> theta_s_cpds(num_states);

            // From HW to HW | RW | SG | SY
            Eigen::MatrixXd theta_s0(1, num_states);
            theta_s0 << 1, 1, EPSILON, EPSILON;
            DirichletCPD theta_s_cpd_temp({}, theta_s0);
            theta_s_cpds[0] = make_shared<DirichletCPD>(move(theta_s_cpd_temp));

            // From RW to HW | RW | SG | SY
            Eigen::MatrixXd theta_s1(1, num_states);
            theta_s1 << 1, 1, 1, 1;
            theta_s_cpd_temp = DirichletCPD({}, theta_s1);
            theta_s_cpds[1] = make_shared<DirichletCPD>(move(theta_s_cpd_temp));

            // From SG to HW | RW | SG | SY
            Eigen::MatrixXd theta_s2(1, num_states);
            theta_s2 << EPSILON, 1, 1, EPSILON;
            theta_s_cpd_temp = DirichletCPD({}, theta_s2);
            theta_s_cpds[2] = make_shared<DirichletCPD>(move(theta_s_cpd_temp));

            // From SY to HW | RW | SG | SY
            Eigen::MatrixXd theta_s3(1, num_states);
            theta_s3 << EPSILON, 1, EPSILON, 1;
            theta_s_cpd_temp = DirichletCPD({}, theta_s3);
            theta_s_cpds[3] = make_shared<DirichletCPD>(move(theta_s_cpd_temp));

            // 1.1.3 Random Variables
            vector<shared_ptr<RandomVariableNode>> theta_s_nodes(
                num_states);
            for (int i = 0; i < num_states; i++) {
                theta_s_nodes[i] =
                    make_shared<RandomVariableNode>(theta_s_metadatas[i]);
                theta_s_nodes[i]->add_cpd_template(theta_s_cpds[i]);
            }

            // 2. Variables

            // 2.1 Metadata
            NodeMetadata state_metadata_temp =
                NodeMetadata::create_multiple_time_link_metadata(
                    STATE, true, false, true, 0, 1, num_states);
            shared_ptr<NodeMetadata> state_metadata =
                make_shared<NodeMetadata>(move(state_metadata_temp));

            NodeMetadata room_metadata_temp =
                NodeMetadata::create_multiple_time_link_metadata(
                    ROOM, true, false, true, 1, 1, 2);
            shared_ptr<NodeMetadata> room_metadata =
                make_shared<NodeMetadata>(move(room_metadata_temp));

            NodeMetadata sg_metadata_temp =
                NodeMetadata::create_multiple_time_link_metadata(
                    SG, true, false, true, 1, 1, 2);
            shared_ptr<NodeMetadata> sg_metadata =
                make_shared<NodeMetadata>(move(sg_metadata_temp));

            NodeMetadata sy_metadata_temp =
                NodeMetadata::create_multiple_time_link_metadata(
                    SY, true, false, true, 1, 1, 2);
            shared_ptr<NodeMetadata> sy_metadata =
                make_shared<NodeMetadata>(move(sy_metadata_temp));

            // 2.2 CPDS

            // 2.2.1 State Prior
            Eigen::MatrixXd state_prior =
                Eigen::MatrixXd::Constant(1, num_states, EPSILON);
            state_prior(0, 0) = 1; // The first state is always 0
            CategoricalCPD state_prior_cpd_temp({}, state_prior);
            shared_ptr<CategoricalCPD> state_prior_cpd =
                make_shared<CategoricalCPD>(move(state_prior_cpd_temp));

            // 2.2.2 State Transition
            vector<shared_ptr<Categorical>> state_transition_matrix;
            state_transition_matrix.reserve(theta_s_nodes.size());
            for (auto& theta_s_node : theta_s_nodes) {
                state_transition_matrix.push_back(
                    make_shared<Categorical>(theta_s_node));
            }
            CategoricalCPD state_transition_cpd_temp({state_metadata},
                                                     state_transition_matrix);
            shared_ptr<CategoricalCPD> state_transition_cpd =
                make_shared<CategoricalCPD>(
                    move(state_transition_cpd_temp));

            // 2.2.3 Room Emission

            // HW (the first state) is the only state where the player is not in
            // a room but in the hallway
            Eigen::MatrixXd room_emission_matrix(num_states, 2);
            room_emission_matrix << 1, EPSILON, EPSILON, 1, EPSILON, 1, EPSILON, 1;
            CategoricalCPD room_emission_cpd_temp({state_metadata},
                                                  room_emission_matrix);
            shared_ptr<CategoricalCPD> room_emission_cpd =
                make_shared<CategoricalCPD>(move(room_emission_cpd_temp));

            // 2.2.4 SG Emission

            Eigen::MatrixXd sg_emission_matrix(num_states, 2);
            sg_emission_matrix << 1, EPSILON, 1, EPSILON, EPSILON, 1, 1, EPSILON;
            CategoricalCPD sg_emission_cpd_temp({state_metadata},
                                                sg_emission_matrix);
            shared_ptr<CategoricalCPD> sg_emission_cpd =
                make_shared<CategoricalCPD>(move(sg_emission_cpd_temp));

            // 2.2.5 SY Emission
            Eigen::MatrixXd sy_emission_matrix(num_states, 2);
            sy_emission_matrix << 1, EPSILON, 1, EPSILON, 1, EPSILON, EPSILON, 1;
            CategoricalCPD sy_emission_cpd_temp({state_metadata},
                                                sy_emission_matrix);
            shared_ptr<CategoricalCPD> sy_emission_cpd =
                make_shared<CategoricalCPD>(move(sy_emission_cpd_temp));

            // 2.3 Random Variables
            RandomVariableNode state(state_metadata);
            state.add_cpd_template(state_prior_cpd);
            state.add_cpd_template(state_transition_cpd);

            RandomVariableNode room(room_metadata);
            room.add_cpd_template(room_emission_cpd);

            RandomVariableNode sg(sg_metadata);
            sg.add_cpd_template(sg_emission_cpd);

            RandomVariableNode sy(sy_metadata);
            sy.add_cpd_template(sy_emission_cpd);

            // 3. Connections
            for (int i = 0; i < num_states; i++) {
                state_metadata->add_parent_link(theta_s_metadatas[i], true);
            }
            state_metadata->add_parent_link(state_metadata, true);
            room_metadata->add_parent_link(state_metadata, false);
            sg_metadata->add_parent_link(state_metadata, false);
            sy_metadata->add_parent_link(state_metadata, false);

            // 4 Dynamic Bayes Net
            DynamicBayesNet dbn;

            // 4.1 Add parameter nodes
            for (int i = 0; i < num_states; i++) {
                dbn.add_node_template(*theta_s_nodes[i]);
            }

            // 4.2 Add variable nodes
            dbn.add_node_template(state);
            dbn.add_node_template(room);
            dbn.add_node_template(sg);
            dbn.add_node_template(sy);

            this->model = make_shared<DynamicBayesNet>(dbn);
            this->model->unroll(600, true);
        }

        void Tomcat::generate_synthetic_data(
            shared_ptr<gsl_rng> random_generator,
            int num_samples,
            const string& output_folder,
            int equal_until) {

            AncestralSampler sampler(model);
            sampler.set_num_in_plate_samples(1);
            sampler.set_equal_samples_time_step_limit(equal_until);
            sampler.sample(random_generator, num_samples);
            sampler.save_samples_to_folder(output_folder);
        }

        //----------------------------------------------------------------------
        // Getters & Setters
        //----------------------------------------------------------------------
        const shared_ptr<DynamicBayesNet>& Tomcat::get_model() const {
            return model;
        }

    } // namespace model
} // namespace tomcat

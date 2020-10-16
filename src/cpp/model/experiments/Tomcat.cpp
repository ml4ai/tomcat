#include "Tomcat.h"

#include <vector>

#include "pgm/NodeMetadata.h"
#include "pgm/RandomVariableNode.h"
#include "pgm/cpd/CategoricalCPD.h"
#include "pgm/cpd/DirichletCPD.h"
#include "sampling/AncestralSampler.h"

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
        void Tomcat::copy_tomcat(const Tomcat& tomcat) {
            this->model = tomcat.model;
        }

        shared_ptr<RandomVariableNode>
        Tomcat::create_node(shared_ptr<NodeMetadata> metadata,
                    vector<std::shared_ptr<CPD>> cpds) const {
            shared_ptr<RandomVariableNode> node =
                make_shared<RandomVariableNode>(metadata);

            for(auto& cpd : cpds){
                node->add_cpd_template(cpd);
            }

            return node;
        }

        void
        Tomcat::generate_synthetic_data(shared_ptr<gsl_rng> random_generator,
                                        int num_samples,
                                        const string& output_folder,
                                        int equal_until,
                                        int max_time_step,
                                        unordered_set<string> excluding) {

            AncestralSampler sampler(model);
            sampler.set_num_in_plate_samples(1);
            sampler.set_equal_samples_time_step_limit(equal_until);
            sampler.set_max_time_step_to_sample(max_time_step);
            sampler.sample(random_generator, num_samples);
            sampler.save_samples_to_folder(output_folder, excluding);
        }

        //----------------------------------------------------------------------
        // Getters & Setters
        //----------------------------------------------------------------------
        const shared_ptr<DynamicBayesNet>& Tomcat::get_model() const {
            return model;
        }

    } // namespace model
} // namespace tomcat

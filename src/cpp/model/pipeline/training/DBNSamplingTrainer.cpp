#include "DBNSamplingTrainer.h"

namespace tomcat {
    namespace model {

        using namespace std;

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        DBNSamplingTrainer::DBNSamplingTrainer(
            shared_ptr<gsl_rng> random_generator,
            shared_ptr<Sampler> sampler,
            int num_samples)
            : random_generator(random_generator), sampler(sampler),
              num_samples(num_samples) {}

        DBNSamplingTrainer::~DBNSamplingTrainer() {}

        //----------------------------------------------------------------------
        // Copy & Move constructors/assignments
        //----------------------------------------------------------------------
        DBNSamplingTrainer::DBNSamplingTrainer(
            const DBNSamplingTrainer& trainer) {
            this->copy_trainer(trainer);
        }

        DBNSamplingTrainer&
        DBNSamplingTrainer::operator=(const DBNSamplingTrainer& trainer) {
            this->copy_trainer(trainer);
            return *this;
        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void
        DBNSamplingTrainer::copy_trainer(const DBNSamplingTrainer& trainer) {
            this->random_generator = trainer.random_generator;
            this->sampler = trainer.sampler;
            this->num_samples = trainer.num_samples;
        }

        void DBNSamplingTrainer::prepare() {}

        void DBNSamplingTrainer::fit(const EvidenceSet& training_data) {
            this->sampler->set_num_in_plate_samples(
                training_data.get_num_data_points());
            this->sampler->add_data(training_data);
            this->sampler->sample(this->random_generator, this->num_samples);

            shared_ptr<DynamicBayesNet> model = this->sampler->get_model();
            unordered_map<string, Tensor3> node_label_to_samples;

            for (const auto& node : model->get_parameter_nodes()) {
                shared_ptr<RandomVariableNode> rv_node =
                    dynamic_pointer_cast<RandomVariableNode>(node);
                if (!rv_node->is_frozen()) {
                    string node_label = node->get_metadata()->get_label();
                    if (!EXISTS(node_label, node_label_to_samples)) {
                        node_label_to_samples[node_label] =
                            this->sampler->get_samples(node_label);
                    }

                    int time_step = rv_node->get_time_step();
                    Eigen::MatrixXd samples =
                        node_label_to_samples[node_label](time_step, 2)
                            .transpose();
                    Eigen::MatrixXd mean_value(1, samples.cols());
                    mean_value = samples.colwise().mean();
                    rv_node->set_assignment(mean_value);
                }
            }
        }

        void DBNSamplingTrainer::get_info(nlohmann::json& json) const {
            json["type"] = "sampling";
            json["num_samples"] = this->num_samples;
            this->sampler->get_info(json["algorithm"]);
        }

    } // namespace model
} // namespace tomcat

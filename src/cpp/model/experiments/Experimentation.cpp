#include "Experimentation.h"

#include <boost/filesystem.hpp>

using namespace std;

namespace tomcat {
    namespace model {

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        Experimentation::Experimentation(shared_ptr<gsl_rng> gen,
                                         const string& experiment_id,
                                         MODEL_VERSION model_version,
                                         const EvidenceSet& training_set,
                                         const EvidenceSet& test_set)
            : gen(gen), experiment_id(experiment_id) {

            this->init_model(model_version);
            this->data_splitter = make_shared<KFold>(training_set, test_set);
            this->offline_estimation = make_shared<OfflineEstimation>();
        }

        Experimentation::Experimentation(shared_ptr<gsl_rng> gen,
                                         const string& experiment_id,
                                         MODEL_VERSION model_version,
                                         const EvidenceSet& data,
                                         int num_folds)
            : gen(gen), experiment_id(experiment_id) {

            this->init_model(model_version);
            this->data_splitter = make_shared<KFold>(data, num_folds, gen);
            this->offline_estimation = make_shared<OfflineEstimation>();
        }

        Experimentation::Experimentation(shared_ptr<gsl_rng> gen,
                                         MODEL_VERSION model_version)
            : gen(gen) {
            this->init_model(model_version);
        }

        Experimentation::~Experimentation() {}

        //----------------------------------------------------------------------
        // Operator overload
        //----------------------------------------------------------------------

        //----------------------------------------------------------------------
        // Static functions
        //----------------------------------------------------------------------

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void Experimentation::init_model(MODEL_VERSION model_version) {
            switch (model_version) {
            case v1: {
                this->tomcat = make_shared<TomcatTA3>();
                this->data_generation_exclusions = {TomcatTA3::STATE};
                break;
            }
            case v2: {
                this->tomcat = make_shared<TomcatTA3V2>();
                this->data_generation_exclusions = {TomcatTA3V2::STATE,
                                                    TomcatTA3V2::PBAE};
                break;
            }
            }
            this->tomcat->init();
        }

        void Experimentation::display_estimates() {
            this->offline_estimation->set_display_estimates(true);
        }

        void Experimentation::load_model_from(const string& input_dir) {
            string final_input_dir = input_dir;

            if (this->data_splitter &&
                this->data_splitter->get_splits().size() > 1) {
                final_input_dir = fmt::format("{}/fold{{}}", input_dir);
            }

            this->trainer = make_shared<DBNLoader>(this->tomcat->get_model(),
                                                   final_input_dir);
        }

        void Experimentation::train_using_gibbs(int burn_in, int num_samples) {
            this->trainer = make_shared<DBNSamplingTrainer>(
                this->gen,
                make_shared<GibbsSampler>(this->tomcat->get_model(), burn_in),
                num_samples);
        }

        void Experimentation::save_model(const string& output_dir) {
            string final_output_dir = output_dir;

            if (this->data_splitter->get_splits().size() > 1) {
                final_output_dir = fmt::format("{}/fold{{}}", final_output_dir);
            }

            this->saver = make_shared<DBNSaver>(this->tomcat->get_model(),
                                                final_output_dir);
        }

        void Experimentation::compute_baseline_estimates_for(
            const string& node_label,
            int inference_horizon,
            Eigen::VectorXd assignment) {

            this->display_estimates();
            shared_ptr<Estimator> estimator =
                make_shared<TrainingFrequencyEstimator>(
                    this->tomcat->get_model(),
                    inference_horizon,
                    node_label,
                    assignment);
            this->offline_estimation->add_estimator(estimator);
        }

        void
        Experimentation::compute_estimates_for(const string& node_label,
                                               int inference_horizon,
                                               Eigen::VectorXd assignment) {

            this->display_estimates();
            shared_ptr<Estimator> estimator =
                make_shared<SumProductEstimator>(this->tomcat->get_model(),
                                                 inference_horizon,
                                                 node_label,
                                                 assignment);
            this->offline_estimation->add_estimator(estimator);
        }

        void Experimentation::compute_baseline_eval_scores_for(
            const string& node_label,
            int inference_horizon,
            vector<MEASURES> measures,
            Eigen::VectorXd assignment) {

            shared_ptr<Estimator> estimator =
                make_shared<TrainingFrequencyEstimator>(
                    this->tomcat->get_model(),
                    inference_horizon,
                    node_label,
                    assignment);

            this->compute_eval_scores_for(
                node_label, inference_horizon, measures, assignment, estimator);
        }

        void Experimentation::compute_eval_scores_for(
            const string& node_label,
            int inference_horizon,
            vector<MEASURES> measures,
            Eigen::VectorXd assignment,
            shared_ptr<Estimator> estimator) {

            if (!measures.empty()) {
                this->init_evaluation();
                bool eval_last_only = this->should_eval_last_only(node_label);

                this->offline_estimation->add_estimator(estimator);

                for (const auto& measure : measures) {
                    if (measure == MEASURES::accuracy) {
                        this->evaluation->add_measure(make_shared<Accuracy>(
                            estimator, 0.5, eval_last_only));
                    }
                    else if (measure == MEASURES::f1) {
                        this->evaluation->add_measure(
                            make_shared<F1Score>(estimator, 0.5));
                    }
                }
            }
        }

        void Experimentation::init_evaluation() {
            if (!this->evaluation) {
                this->evaluation = make_shared<EvaluationAggregator>(
                    EvaluationAggregator::METHOD::no_aggregation);
            }
        }

        bool Experimentation::should_eval_last_only(const string& node_label) {
            // If the node is not repeatable in the unrolled DBN, we evaluate
            // the accuracy using the estimate in the last time step which will
            // give us the distribution of the node given all the observations
            // in the mission trial.
            return this->tomcat->get_model()
                       ->get_nodes_by_label(node_label)
                       .size() == 1;
        }

        void
        Experimentation::compute_eval_scores_for(const string& node_label,
                                                 int inference_horizon,
                                                 vector<MEASURES> measures,
                                                 Eigen::VectorXd assignment) {

            shared_ptr<Estimator> estimator =
                make_shared<SumProductEstimator>(this->tomcat->get_model(),
                                                 inference_horizon,
                                                 node_label,
                                                 assignment);

            this->compute_eval_scores_for(
                node_label, inference_horizon, measures, assignment, estimator);
        }

        void Experimentation::train_and_evaluate(const string& output_dir) {
            boost::filesystem::create_directories(output_dir);
            string filepath = fmt::format("{}/evaluations.json", output_dir);
            ofstream output_file;
            output_file.open(filepath);
            Pipeline pipeline(this->experiment_id, output_file);
            pipeline.set_data_splitter(this->data_splitter);
            pipeline.set_model_trainer(this->trainer);
            pipeline.set_model_saver(this->saver);
            pipeline.set_estimation_process(this->offline_estimation);
            pipeline.set_aggregator(this->evaluation);
            pipeline.execute();
        }

        void Experimentation::train_and_save() {
            for (const auto& [training_data, test_data] :
                 this->data_splitter->get_splits()) {
                this->trainer->prepare();
                this->trainer->fit(training_data);
                this->saver->save();
            }
        }

        void Experimentation::generate_synthetic_data(
            int num_samples, const std::string& output_dir, int equals_until, int max_time_step) {

            this->trainer->fit({});
            this->tomcat->generate_synthetic_data(
                this->gen,
                num_samples,
                output_dir,
                equals_until,
                max_time_step,
                this->data_generation_exclusions);
        }

        //----------------------------------------------------------------------
        // Getters & Setters
        //----------------------------------------------------------------------

    } // namespace model
} // namespace tomcat

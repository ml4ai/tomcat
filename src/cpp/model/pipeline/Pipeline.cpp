#include "Pipeline.h"
#include <nlohmann/json.hpp>

namespace tomcat {
    namespace model {

        using namespace std;

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        Pipeline::Pipeline(ostream& output_stream)
            : output_stream(output_stream) {}

        Pipeline::Pipeline(const string& id, ostream& output_stream)
            : id(id), output_stream(output_stream) {}

        Pipeline::~Pipeline() {}

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void Pipeline::execute() {
            this->check();

            boost::posix_time::ptime start_time =
                boost::posix_time::microsec_clock::universal_time();

            this->estimation_process->clear_estimates();
            if (this->evaluation != nullptr) {
                this->evaluation->clear_evaluations();
            }

            vector<KFold::Split> splits = this->data_splitter->get_splits();
            for (const auto& [training_data, test_data] : splits) {
                this->model_trainer->prepare();
                this->model_trainer->fit(training_data);
                if (this->model_saver != nullptr) {
                    this->model_saver->save();
                }

                this->estimation_process->prepare();
                this->estimation_process->set_training_data(training_data);
                this->estimation_process->estimate(test_data);
                this->estimation_process->keep_estimates();

                if (this->evaluation != nullptr) {
                    this->evaluation->evaluate(test_data);
                }
            }

            if (this->evaluation != nullptr) {
                this->evaluation->aggregate();
            }

            boost::posix_time::ptime end_time =
                boost::posix_time::microsec_clock::universal_time();

            this->display_results(start_time, end_time);
        }

        void Pipeline::check() {
            if (this->data_splitter == nullptr) {
                throw TomcatModelException(
                    "A data splitter was not provided to the pipeline.");
            }

            if (this->model_trainer == nullptr) {
                throw TomcatModelException(
                    "A model trainer was not provided to the pipeline.");
            }

            if (this->estimation_process == nullptr) {
                LOG_WARNING("No estimation was provided to the pipeline.");
            }
        }

        void Pipeline::display_results(
            const boost::posix_time::ptime& execution_start_time,
            const boost::posix_time::ptime& execution_end_time) {

            string initial_timestamp =
                boost::posix_time::to_iso_extended_string(execution_start_time);
            string final_timestamp =
                boost::posix_time::to_iso_extended_string(execution_end_time);
            auto duration = execution_end_time - execution_start_time;
            long duration_in_seconds = duration.total_seconds();

            nlohmann::json json;
            json["id"] = this->id;
            json["execution_start"] = initial_timestamp;
            json["execution_end"] = final_timestamp;
            json["duration_in_seconds"] = duration_in_seconds;
            this->data_splitter->get_info(json["data_split"]);
            this->model_trainer->get_info(json["training"]);
            this->estimation_process->get_info(json["estimation"]);
            if (this->evaluation) {
                this->evaluation->get_info(json["evaluation"]);
            }

            this->output_stream << setw(4) << json;
        }

        //----------------------------------------------------------------------
        // Getters & Setters
        //----------------------------------------------------------------------
        void Pipeline::set_data_splitter(
            const shared_ptr<KFold>& data_splitter) {
            this->data_splitter = data_splitter;
        }

        void Pipeline::set_model_trainer(
            const shared_ptr<DBNTrainer>& model_trainer) {
            this->model_trainer = model_trainer;
        }

        void Pipeline::set_model_saver(
            const shared_ptr<DBNSaver>& model_saver) {
            this->model_saver = model_saver;
        }

        void Pipeline::set_estimation_process(
            const shared_ptr<EstimationProcess>& estimation_process) {
            this->estimation_process = estimation_process;
        }

        void Pipeline::set_aggregator(
            const shared_ptr<EvaluationAggregator>& aggregator) {
            this->evaluation = aggregator;
        }

    } // namespace model
} // namespace tomcat

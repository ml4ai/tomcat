#include "Pipeline.h"

namespace tomcat {
    namespace model {

        //----------------------------------------------------------------------
        // Definitions
        //----------------------------------------------------------------------

        // No definitions in this file

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        Pipeline::Pipeline() {}

        Pipeline::Pipeline(const std::string& id) : id(id) {}

        Pipeline::~Pipeline() {}

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void Pipeline::execute() {
            KFold::Split splits = this->data_splitter->split(this->data);
            for (const auto& [training_data, test_data] : splits) {
                this->model_trainner->fit(training_data);
                if (this->model_saver != nullptr) {
                    this->model_saver->save();
                }
                std::vector<std::thread> threads =
                    this->start_estimation_threads(test_data);
                for (auto& thread : threads) {
                    thread.join();
                }
                if (this->aggregator != nullptr) {
                    this->aggregator->aggregate(test_data);
                }
            }
            if (this->aggregator != nullptr) {
                this->aggregator->dump();
            }
        }

        std::vector<std::thread>
        Pipeline::start_estimation_threads(const EvidenceSet& test_data) {
            std::vector<std::thread> threads;
            threads.reserve(this->estimations.size());
            for (auto& estimation : this->estimations) {
                estimation->reset();
                std::thread estimation_thread(
                    &Pipeline::estimate, this, estimation, test_data);
                threads.push_back(std::move(estimation_thread));
            }

            return threads;
        }

        void Pipeline::estimate(std::shared_ptr<Estimation> estimation,
                                const EvidenceSet& test_data) {
            while (!estimation->is_finished()) {
                estimation->estimate(test_data);
            }
        }

        void Pipeline::add_estimation(
            const std::shared_ptr<Estimation>& estimation) {
            this->estimations.push_back(estimation);
        }

        //----------------------------------------------------------------------
        // Getters & Setters
        //----------------------------------------------------------------------
        void Pipeline::set_data(const EvidenceSet& data) { this->data = data; }

        void Pipeline::set_data_splitter(
            const std::shared_ptr<KFold>& data_splitter) {
            this->data_splitter = data_splitter;
        }

        void Pipeline::set_model_trainner(
            const std::shared_ptr<DBNTrainer>& model_trainner) {
            this->model_trainner = model_trainner;
        }

        void Pipeline::set_model_saver(
            const std::shared_ptr<DBNSaver>& model_saver) {
            this->model_saver = model_saver;
        }

        void Pipeline::set_aggregator(
            const std::shared_ptr<MeasureAggregator>& aggregator) {
            this->aggregator = aggregator;
        }

    } // namespace model
} // namespace tomcat

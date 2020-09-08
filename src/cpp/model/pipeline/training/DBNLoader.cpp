#include "DBNLoader.h"

#include <fmt/format.h>

namespace tomcat {
    namespace model {

        using namespace std;

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        DBNLoader::DBNLoader(shared_ptr<DynamicBayesNet> model,
                             string input_folder_path)
            : model(model), input_folder_path(input_folder_path) {}

        DBNLoader::~DBNLoader() {}

        //----------------------------------------------------------------------
        // Copy & Move constructors/assignments
        //----------------------------------------------------------------------
        DBNLoader::DBNLoader(const DBNLoader& loader) {
            this->copy_loader(loader);
        }

        DBNLoader& DBNLoader::operator=(const DBNLoader& loader) {
            this->copy_loader(loader);
            return *this;
        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void DBNLoader::copy_loader(const DBNLoader& loader) {
            this->model = loader.model;
            this->input_folder_path = loader.input_folder_path;
            this->cv_step = loader.cv_step;
        }

        void DBNLoader::prepare() { this->cv_step = 0; }

        void DBNLoader::fit(const EvidenceSet& training_data) {
            // If the name of the folder has a placeholder for the cv step,
            // replace it with the current number.
            string final_folder_path =
                fmt::format(this->input_folder_path, ++this->cv_step);
            this->model->load_from_folder(final_folder_path);
        }

        void DBNLoader::get_info(nlohmann::json& json) const {
            json["type"] = "pre_trained";
            json["input_folder_path"] = this->input_folder_path;
        }

    } // namespace model
} // namespace tomcat

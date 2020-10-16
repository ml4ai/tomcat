#include "DBNSaver.h"

#include <fmt/format.h>

namespace tomcat {
    namespace model {

        using namespace std;

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        DBNSaver::DBNSaver(shared_ptr<DynamicBayesNet> model,
                           string output_folder_path)
            : model(model), output_folder_path(output_folder_path) {}

        DBNSaver::~DBNSaver() {}

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void DBNSaver::prepare() { this->cv_step = 0; }

        void DBNSaver::save() {
            // If the name of the folder has a placeholder for the cv step,
            // replace it with the current number.
            string final_folder_path =
                fmt::format(this->output_folder_path, ++this->cv_step);
            this->model->save_to(final_folder_path);
        }

    } // namespace model
} // namespace tomcat

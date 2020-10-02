#pragma once

#include <memory>
#include <string>

#include "DBNTrainer.h"

#include "utils/Definitions.h"
#include "pgm/DynamicBayesNet.h"

namespace tomcat {
    namespace model {

        /**
         * Class responsible for loading a model from a pre-trained set of
         * parameters.
         */
        class DBNLoader : public DBNTrainer {
          public:
            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates an instance of the loader.
             *
             * @param model: DBN
             * @param input_folder_path: folder where the model's parameters
             * are stored
             */
            DBNLoader(std::shared_ptr<DynamicBayesNet> model,
                      std::string input_folder_path);

            ~DBNLoader();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------
            DBNLoader(const DBNLoader& loader);

            DBNLoader& operator=(const DBNLoader& loader);

            DBNLoader(DBNLoader&&) = default;

            DBNLoader& operator=(DBNLoader&&) = default;

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------
            void prepare() override;

            void fit(const EvidenceSet& training_data) override;

            void get_info(nlohmann::json& json) const override;

          private:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Copies data members from another DBNLoader.
             */
            void copy_loader(const DBNLoader& loader);

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------
            std::shared_ptr<DynamicBayesNet> model;

            // Folder where the model's parameters' files are saved.
            std::string input_folder_path;

            // Cross validation step. This is incremented at each call of the
            // function fit. It can be used to identify a folder with parameters
            // for a specific cross validation step.
            int cv_step = 0;
        };

    } // namespace model
} // namespace tomcat

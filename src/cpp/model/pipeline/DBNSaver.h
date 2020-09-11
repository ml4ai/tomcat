#pragma once

#include <memory>
#include <string>

#include "model/utils/Definitions.h"
#include "model/pgm/DynamicBayesNet.h"

namespace tomcat {
    namespace model {

        /**
         * Class responsible to save a model's parameters to files in a specific
         * folder.
         */
        class DBNSaver {
          public:
            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates an instance of the saver.
             *
             * @param model: DBN
             * @param output_folder_path: folder where the model's parameters
             * must be saved in
             */
            DBNSaver(std::shared_ptr<DynamicBayesNet> model,
                     std::string output_folder_path);

            ~DBNSaver();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------

            // Copy constructor and assignment should be deleted to avoid
            // implicit slicing and loss of polymorphic behaviour in the
            // subclasses. To deep copy, the clone method must be used.
            DBNSaver(const DBNSaver&) = delete;

            DBNSaver& operator=(const DBNSaver&) = delete;

            DBNSaver(DBNSaver&&) = default;

            DBNSaver& operator=(DBNSaver&&) = default;

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Prepares the trainer to a series of calls to the function fit by
             * performing necessary cleanups.
             */
            void prepare();

            /**
             * Saves a model's parameters into files in a specific folder.
             */
            void save();

          private:
            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------
            std::shared_ptr<DynamicBayesNet> model;

            // Folder where the model's parameters' files will be saved.
            std::string output_folder_path;

            // Cross validation step. This is incremented at each call of the
            // function save. It can be used to identify a folder with
            // parameters for a specific cross validation step.
            int cv_step = 0;
        };

    } // namespace model
} // namespace tomcat

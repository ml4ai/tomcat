#pragma once

#include <string>
#include <unordered_map>

#include "FileHandler.h"

namespace tomcat {
    namespace model {

        /**
         * This class contains observations for data nodes in a DBN. The
         * observations are stored in three dimensional tensors (sample size,
         * number data points, time steps).
         */
        class EvidenceSet {
          public:
            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates an evidence set with data from files in a given folder.
             *
             * @param data_folder_path: folder where files with nodes'
             * observations are stored.
             */
            EvidenceSet(const std::string& data_folder_path);

            ~EvidenceSet();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------
            EvidenceSet(const EvidenceSet&) = default;

            EvidenceSet& operator=(const EvidenceSet&) = default;

            EvidenceSet(EvidenceSet&&) = default;

            EvidenceSet& operator=(EvidenceSet&&) = default;

            //------------------------------------------------------------------
            // Operator overload
            //------------------------------------------------------------------

            /**
             * Returns data for a given node.
             *
             * @param node_label: node's label
             *
             * @return Observations for the informed node.
             */
            const Tensor3& operator[](const std::string& node_label);

            /**
             * Returns data for a given node.
             *
             * @param node_label: node's label
             *
             * @return Observations for the informed node.
             */
            const Tensor3& operator[](std::string&& node_label);

            //------------------------------------------------------------------
            // Getters & Setters
            //------------------------------------------------------------------
            int get_num_data_points() const;

            int get_time_steps() const;

          private:
            /**
             * Reads data from files in a folder and store in the evidence set.
             *
             * @param data_folder_path: folder where the data files are stored
             */
            void init_from_folder(const std::string& data_folder_path);

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------
            int num_data_points = 0;

            int time_steps = 0;

            std::unordered_map<std::string, Tensor3> node_label_to_data;
        };

    } // namespace model
} // namespace tomcat

#pragma once

#include <string>
#include <unordered_map>

#include "../utils/Definitions.h"
#include "../utils/FileHandler.h"

namespace tomcat {
    namespace model {

        /**
         * This class contains a map between node labels in a DBN and values
         * stored into 3-dimensional tensors (value dimensionality, number data
         * points, time steps).
         */
        class DBNData {
          public:
            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates a blank DBNData object.
             *
             */
            DBNData();

            /**
             * Creates an DBNData object with data from files in a given folder.
             *
             * @param data_folder_path: folder where files with nodes'
             * values are stored.
             */
            DBNData(const std::string& data_folder_path);

            ~DBNData();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------
            DBNData(const DBNData&) = default;

            DBNData& operator=(const DBNData&) = default;

            DBNData(DBNData&&) = default;

            DBNData& operator=(DBNData&&) = default;

            //------------------------------------------------------------------
            // Operator overload
            //------------------------------------------------------------------

            /**
             * Returns data for a given node.
             *
             * @param node_label: node's label
             *
             * @return Values for the informed node.
             */
            const Tensor3& operator[](const std::string& node_label) const;

            /**
             * Returns data for a given node.
             *
             * @param node_label: node's label
             *
             * @return Values for the informed node.
             */
            const Tensor3& operator[](std::string&& node_label) const;

            /**
             * Returns the labels of the nodes which the DBNData has values
             * for.
             *
             * @return Nodes' labels.
             */
            std::vector<std::string> get_node_labels() const;

            /**
             * Adds data for a specific node.
             *
             * @param node_label: node's label
             * @param data: values for the node
             */
            void add_data(const std::string& node_label, const Tensor3& data);

            /**
             * Checks whether this object contains data for a given node.
             *
             * @param node_label: node's label
             *
             * @return: Whether this object contains data for a given node.
             */
            bool has_data_for(const std::string& node_label);

            /**
             * Assigns a new tensor to a node;
             *
             * @param node_label: node's label
             *
             */
            void set_data_for(const std::string& node_label,
                                       const Tensor3 data);

            //------------------------------------------------------------------
            // Getters & Setters
            //------------------------------------------------------------------
            int get_num_data_points() const;

            int get_time_steps() const;

          private:
            /**
             * Reads data from files in a folder and store in this object.
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

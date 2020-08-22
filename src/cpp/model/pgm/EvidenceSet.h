#pragma once

#include <string>
#include <unordered_map>

#include <nlohmann/json.hpp>

#include "../utils/Definitions.h"
#include "../utils/FileHandler.h"

namespace tomcat {
    namespace model {

        /**
         * This class contains a map between node labels in a DBN and values
         * stored into 3-dimensional tensors (value dimensionality, number data
         * points, time steps).
         */
        class EvidenceSet {
          public:
            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates a blank DBNData object.             *
             */
            EvidenceSet();

            /**
             * Creates an DBNData object with data from files in a given folder.
             *
             * @param data_folder_path: folder where files with nodes'
             * values are stored.
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

            /**
             * Writes information about the splitter in a json object.
             *
             * @param json: json object
             */
            void get_info(nlohmann::json& json) const;

            //------------------------------------------------------------------
            // Getters & Setters
            //------------------------------------------------------------------
            int get_num_data_points() const;

            int get_time_steps() const;

            void set_id(const std::string& id);

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
            std::string id = "";

            int num_data_points = 0;

            int time_steps = 0;

            std::unordered_map<std::string, Tensor3> node_label_to_data;
        };

    } // namespace model
} // namespace tomcat

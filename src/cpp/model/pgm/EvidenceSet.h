#pragma once

#include <string>
#include <unordered_map>

#include <nlohmann/json.hpp>

#include "utils/Definitions.h"
#include "utils/FileHandler.h"

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

            //------------------------------------------------------------------
            // Static functions
            //------------------------------------------------------------------

            /**
             * Returns a matrix such that, for each coefficient in the original
             * matrix up to the original number of columns - window, 1 will be
             * assigned to the coefficient if a given assignment was observed
             * within a given window (if the assignment shows up in at least
             * one of the subsequent columns of the coefficient up to window
             * size), 0 otherwise. The assignment is compared with the elements
             * in the first dimension of the tensor. Therefore the result will
             * be a matrix.
             *
             * @param data: data
             * @param assignment: assignment to compare against
             * @param window: determines the number of columns to look ahead (it
             * only looks ahead for windows of size > 1)
             *
             * @return Logical matrix with observations within a window.
             */
            static Eigen::MatrixXd
            get_observations_in_window(const Tensor3& data,
                                       const Eigen::VectorXd& assignment,
                                       int window);
            /**
             * Returns the first time step with values different than NO_OBS for
             * some node's data.
             *
             * @param data: data
             *
             * @return First time step with actual data.
             */
            static int get_first_time_with_observation(const Tensor3& data);

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

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
            bool has_data_for(const std::string& node_label) const;

            /**
             * Assigns a new tensor to a node;
             *
             * @param node_label: node's label
             *
             */
            void set_data_for(const std::string& node_label,
                              const Tensor3 data);

            /**
             * For a given node's data, returns a logical matrix flagging the
             * time steps where a given assignment was observed within a given
             * window.
             *
             * @param node_label: node's label
             * @param assignment: assignment to compare against
             * @param window: determines the number of columns to look ahead (it
             * only looks ahead for windows of size > 1)
             *
             * @return Logical matrix with observations within a window.
             */
            Eigen::MatrixXd
            get_observations_in_window_for(const std::string& node_label,
                                           const Eigen::VectorXd& assignment,
                                           int window) const;

            /**
             * Keep the first samples and remove the rest.
             *
             * @param num_samples: Number of the first samples to preserve.
             */
            void keep_first(int num_samples);

            /**
             * Shrinks the data up to a time slice (inclusive).
             *
             * @param time_slice: max time step included in the data.
             */
            void shrink_up_to(int time_step);

            /**
             * Indicates whether the set has data or not.
             *
             * @return True if the set has any data point.
             */
            bool empty() const;

            /**
             * Removes data for a given node.
             *
             * @param node_label: nodes' label
             */
            void remove(const std::string& node_label);

            //------------------------------------------------------------------
            // Getters & Setters
            //------------------------------------------------------------------
            int get_num_data_points() const;

            int get_time_steps() const;

          private:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

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

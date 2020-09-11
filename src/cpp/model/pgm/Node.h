#pragma once

#include <unordered_map>

#include <eigen3/Eigen/Dense>

#include "model/utils/Definitions.h"
#include "model/pgm/NodeMetadata.h"

namespace tomcat {
    namespace model {

        /**
         * A node in a Dynamic Bayes Net (DBN). A node can either be a data node
         * or a parameter node. The former defines observed and latent nodes in
         * a PGM, the latter defines nodes that store parameters for some data
         * node's distributions.
         */
        class Node {
          public:
            //------------------------------------------------------------------
            // Types, Enums & Constants
            //------------------------------------------------------------------
            typedef std::unordered_map<std::string, std::shared_ptr<Node>>
                NodeMap;

            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates an abstract representation of a node.
             */
            Node();

            /**
             * Creates an abstract representation of a node with associated
             * metadata.
             *
             * @param metadata: node's metadata
             */
            Node(std::shared_ptr<NodeMetadata> metadata);

            virtual ~Node();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------

            // Copy constructor and assignment should be deleted to avoid
            // implicit slicing and loss of polymorphic behaviour in the
            // subclasses. To deep copy, the clone method must be used.
            Node(const Node&) = delete;

            Node& operator=(const Node&) = delete;

            Node(Node&&) = default;

            Node& operator=(Node&&) = default;

            //------------------------------------------------------------------
            // Operator overload
            //------------------------------------------------------------------
            friend std::ostream& operator<<(std::ostream& os, const Node& node);

            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Prints a short description of the node.
             *
             * @param os: output stream
             */
            void print(std::ostream& os) const;

            /**
             * Returns the number of assigned values in the node.
             *
             * @return Number of assigned values in the node.
             */
            int get_size() const;

            //------------------------------------------------------------------
            // Pure virtual functions
            //------------------------------------------------------------------

            /**
             * Creates a new unique pointer from a concrete instance of a node.
             *
             * @return pointer to the new node
             */
            virtual std::unique_ptr<Node> clone() const = 0;

            /**
             * Returns the node's unique id in an unrolled DBN. This id is a
             * combination of the node's label and the time step where the timed
             * instance node was placed in the unrolled DBN. If the node was not
             * added to a DBN or this was not yet unrolled, the time step of the
             * node is its default value.
             *
             * @return Timed-instance node's description in an unrolled DBN.
             */
            virtual std::string get_timed_name() const = 0;

            /**
             * Returns the node's unique id in an unrolled DBN for an arbitrary
             * time step.
             *
             * @return Node's description in an unrolled DBN for an arbitrary
             * time step.
             */
            // virtual std::string get_timed_name(int time_step) const = 0;

            // --------------------------------------------------------
            // Getters & Setters
            // --------------------------------------------------------
            const std::shared_ptr<NodeMetadata>& get_metadata() const;

            const Eigen::MatrixXd& get_assignment() const;

          protected:
            //------------------------------------------------------------------
            // Pure virtual functions
            //------------------------------------------------------------------

            /**
             * Returns a short description of the node.
             *
             * @return Node's description.
             */
            virtual std::string get_description() const = 0;

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------

            // Metadata is a shared pointer because each timed instance of a
            // node in the unrolled DBN will share the same metadata.
            std::shared_ptr<NodeMetadata> metadata;

            // Multiple values can be assigned to a node (e.g. when the node is
            // observable and data is provided). Each value is assigned to a row
            // of the matrix.
            Eigen::MatrixXd assignment;
        };

    } // namespace model
} // namespace tomcat

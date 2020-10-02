#pragma once

#include <string>
#include <unordered_map>
#include <vector>

#include <nlohmann/json.hpp>

#include "utils/Definitions.h"

namespace tomcat {
    namespace model {

        /**
         * Generic class for converting messages related to the observations
         * from Minecraft to a matrix format that can be read by the model.
         */
        class MessageConverter {
          public:
            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates an empty abstract message converter.
             */
            MessageConverter();

            /**
             * Creates an abstract instance of the message converter.
             *
             * @param time_gap: gap (in seconds) between observations.
             */
            MessageConverter(int time_gap);

            virtual ~MessageConverter();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------

            // Copy constructor and assignment should be deleted to avoid
            // implicit slicing and loss of polymorphic behaviour in the
            // subclasses. To deep copy, the clone method must be used.
            MessageConverter(const MessageConverter&) = delete;

            MessageConverter& operator=(const MessageConverter&) = delete;

            MessageConverter(MessageConverter&&) = default;

            MessageConverter& operator=(MessageConverter&&) = default;

            //------------------------------------------------------------------
            // Pure virtual functions
            //------------------------------------------------------------------

            /**
             * Converts messages from files in a given folder to files for each
             * observable node, consisting of tensors with the observations for
             * each mission sample and time step in the mission.
             *
             * @param input_dir: directory where the testbed message files are
             * stored
             * @param output_dir: directory where node's observations over
             * mission samples and time steps must be saved
             */
            virtual void convert_offline(const std::string& input_dir,
                                 const std::string& output_dir) = 0;

            /**
             * Parses a json message object and, if related to an observation of
             * a given node, returns it values. If it's a world observation,
             * updates the time step. Otherwise, ignore the message.
             *
             * @param message: json object as a string
             *
             * @return List of pairs containing a nodes' labels and observations
             * for the current time step.
             */
            virtual std::unordered_map<std::string, double>
            convert_online(const nlohmann::json& message) = 0;

          protected:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Copies the data members from another converter instance.
             *
             * @param converter: converter to copy the data members from
             */
            void copy_converter(const MessageConverter& converter);

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------

            // Number of seconds between observations
            int time_gap;

            // Next time step to gather observations from
            int time_step = 0;
        };

    } // namespace model
} // namespace tomcat

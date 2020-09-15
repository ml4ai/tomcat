#pragma once

#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

#include <boost/optional.hpp>
#include <nlohmann/json.hpp>

#include "model/utils/Definitions.h"

namespace tomcat {
    namespace model {

        /**
         * Class description here
         */
        class TA3MessageConverter {
          public:
            //------------------------------------------------------------------
            // Types, Enums & Constants
            //------------------------------------------------------------------
            // Total number of time steps in a mission.
            inline static const int T = 600;

            // Node labels
            inline static const std::string ROOM = "Room";
            inline static const std::string SG = "Green";
            inline static const std::string SY = "Yellow";
            inline static const std::string Q = "TrainingCondition";
            inline static const std::string BEEP = "Beep";

            //------------------------------------------------------------------
            // Constructors & Destructor
            //------------------------------------------------------------------

            /**
             * Creates an empty message converter.
             */
            TA3MessageConverter();

            /**
             * Creates an instance of the TA3 message converter.
             *
             * @param map_config_filepath: path of the map configuration file
             * @param time_gap: gap (in seconds) between observations.
             */
            TA3MessageConverter(const std::string& map_config_filepath,
                                int time_gap = 1);

            ~TA3MessageConverter();

            //------------------------------------------------------------------
            // Copy & Move constructors/assignments
            //------------------------------------------------------------------
            TA3MessageConverter(const TA3MessageConverter&) = default;

            TA3MessageConverter&
            operator=(const TA3MessageConverter&) = default;

            TA3MessageConverter(TA3MessageConverter&&) = default;

            TA3MessageConverter& operator=(TA3MessageConverter&&) = default;

            //------------------------------------------------------------------
            // Operator overload
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Static functions
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Member functions
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
            void convert_offline(const std::string& input_dir,
                                 const std::string& output_dir);

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
            std::unordered_map<std::string, double>
            convert_online(const nlohmann::json& message);

            //------------------------------------------------------------------
            // Virtual functions
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Pure virtual functions
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Getters & Setters
            //------------------------------------------------------------------

          protected:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------

          private:
            //------------------------------------------------------------------
            // Member functions
            //------------------------------------------------------------------

            /**
             * Fill the map of observations with default not observed values.
             */
            void init_observations();

            /**
             * Loads map of area configuration as a hash map to easily determine
             * if an area is a room or not by its id.
             *
             * @param map_config_filepath: path of the map configuration file
             */
            void
            load_map_area_configuration(const std::string& map_config_filepath);

            /**
             * Reads messages from a file and returns them sorted by timestamp.
             *
             * @param filepath: path of the file where the messages are stored.
             *
             * @return Sorted messages
             */
            std::vector<nlohmann::json>
            get_sorted_messages_in(const std::string& filepath);

            /**
             * Converts a string with remaining minutes and seconds to the total
             * number of seconds for the mission to end.
             *
             * @param time: string containing the remaining time formatted (mm :
             * ss)
             *
             * @return Remaining time in seconds.
             */
            int get_remaining_seconds_from(const std::string& time);

            /**
             * Returns observation related to victim saving.
             *
             * @param json_message: json message containing a victim saving
             * event.
             *
             * @return Victim saving event observation.
             */
            void
            fill_victim_saving_observation(
                const nlohmann::json& json_message);

            /**
             * Returns observation related to being in a room or not.
             *
             * @param json_message: json message containing information about
             * the room the player is in.
             *
             * @return Room observation.
             */
            void fill_room_observation(const nlohmann::json& json_message);

            /**
             * Returns observation about the training condition used in the
             * experiment.
             *
             * @param json_message: json message containing information about
             * the training condition.
             *
             * @return Training condition observation.
             */
            void fill_training_condition_observation(
                const nlohmann::json& json_message);

            /**
             * Returns observation about the beep event.
             *
             * @param json_message: json message containing information about
             * the beep played.
             *
             * @return Observation regarding the beep playing.
             */
            void fill_beep_observation(const nlohmann::json& json_message);

            //------------------------------------------------------------------
            // Data members
            //------------------------------------------------------------------

            // Number of seconds between observations
            int time_gap = 1;

            int time_step = 0;

            // Indicates whether a message informing about the mission start was
            // received. Messages received before the mission starts will be
            // ignored.
            bool mission_started = false;

            // Stores the id of all possible areas in the map along with a flag
            // indicating whether the area is a room or not (e.g, yard, hallway
            // etc.).
            std::unordered_map<std::string, bool> map_area_configuration;

            // Last observations per node.
            std::unordered_map<std::string, double> last_observations_per_node;
        };

    } // namespace model
} // namespace tomcat

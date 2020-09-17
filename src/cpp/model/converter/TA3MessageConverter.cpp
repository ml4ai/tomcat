#include "TA3MessageConverter.h"

#include <fstream>
#include <iostream>
#include <sstream>

#include <boost/filesystem.hpp>
#include <boost/progress.hpp>
#include <eigen3/Eigen/Dense>

#include "model/utils/FileHandler.h"

using namespace std;
namespace fs = boost::filesystem;

namespace tomcat {
    namespace model {

        //----------------------------------------------------------------------
        // Constructors & Destructor
        //----------------------------------------------------------------------
        TA3MessageConverter::TA3MessageConverter(
            const std::string& map_config_filepath, int time_gap)
            : MessageConverter(time_gap) {
            this->init_observations();
            this->load_map_area_configuration(map_config_filepath);
        }

        TA3MessageConverter::~TA3MessageConverter() {}

        //----------------------------------------------------------------------
        // Copy & Move constructors/assignments
        //----------------------------------------------------------------------
        TA3MessageConverter::TA3MessageConverter(
            const TA3MessageConverter& converter) {
            this->copy_converter(converter);
        }

        TA3MessageConverter&
        TA3MessageConverter::operator=(const TA3MessageConverter& converter) {
            this->copy_converter(converter);
            return *this;
        }

        //----------------------------------------------------------------------
        // Member functions
        //----------------------------------------------------------------------
        void TA3MessageConverter::init_observations() {
            this->last_observations_per_node[ROOM] = NO_OBS;
            this->last_observations_per_node[SG] = NO_OBS;
            this->last_observations_per_node[SY] = NO_OBS;
            this->last_observations_per_node[Q] = NO_OBS;
            this->last_observations_per_node[BEEP] = NO_OBS;
        }

        void TA3MessageConverter::load_map_area_configuration(
            const std::string& map_config_filepath) {
            fstream map_config_file;
            map_config_file.open(map_config_filepath);
            if (map_config_file.is_open()) {
                nlohmann::json json_map_config =
                    nlohmann::json::parse(map_config_file);
                for (const auto& location : json_map_config["locations"]) {
                    const string area_type = location["type"];
                    this->map_area_configuration[location["id"]] =
                        area_type.find("room") != string::npos;
                }
            }
            else {
                stringstream ss;
                ss << "Map configuration file in " << map_config_filepath
                   << " does not exist.";
                throw TomcatModelException(ss.str());
            }
        }

        void
        TA3MessageConverter::convert_offline(const std::string& input_dir,
                                             const std::string& output_dir) {

            int num_mission_trials = std::count_if(
                fs::directory_iterator(input_dir),
                fs::directory_iterator(),
                static_cast<bool (*)(const fs::path&)>(fs::is_regular_file));
            int d = 0;
            unordered_map<string, Eigen::MatrixXd> observations_per_node;
            boost::progress_display progress(num_mission_trials);

            for (const auto& file : fs::directory_iterator(input_dir)) {
                if (fs::is_regular_file(file)) {
                    vector<nlohmann::json> messages =
                        this->get_sorted_messages_in(file.path().string());

                    for (auto& message : messages) {
                        for (const auto& [node_label, value] :
                             this->convert_online(message)) {

                            // Initialize matrix of observations with non
                            // observed value
                            if (!EXISTS(node_label, observations_per_node)) {
                                observations_per_node[node_label] =
                                    Eigen::MatrixXd::Constant(
                                        num_mission_trials, T+1, NO_OBS);
                            }

                            if (this->time_step <= T) {
                                observations_per_node[node_label](
                                    d, this->time_step) = value;
                            }
                        }

                        if (this->time_step > T) {
                            break;
                        }
                    }
                    d++;
                    ++progress;
                    this->time_step = 0;
                    this->mission_started = false;
                    this->init_observations();
                }
            }

            boost::filesystem::create_directories(output_dir);

            for (const auto& [node_label, data_matrix] :
                 observations_per_node) {
                stringstream output_filepath;
                output_filepath << output_dir << "/" << node_label << ".txt";
                save_matrix_to_file(output_filepath.str(), data_matrix);
            }
        }

        vector<nlohmann::json> TA3MessageConverter::get_sorted_messages_in(
            const std::string& filepath) {
            vector<nlohmann::json> messages;

            ifstream file_reader(filepath);
            while (!file_reader.eof()) {
                string message;
                getline(file_reader, message);
                messages.push_back(nlohmann::json::parse(message));
            }

            std::sort(messages.begin(),
                      messages.end(),
                      [](const nlohmann::json& lhs, const nlohmann::json& rhs) {
                          return lhs["header"]["timestamp"].dump() <
                                 rhs["header"]["timestamp"].dump();
                      });

            return messages;
        }

        unordered_map<string, double>
        TA3MessageConverter::convert_online(const nlohmann::json& message) {
            unordered_map<string, double> observations_per_node;

            if (!this->mission_started) {
                if (message["topic"] == "observations/events/mission" &&
                    message["data"]["mission_state"] == "Start") {

                    this->mission_started = true;
                    observations_per_node = this->last_observations_per_node;
                }
                else if (message["topic"] == "trial") {
                    this->fill_training_condition_observation(message);
                }
            }

            if (this->mission_started) {
                if (message["topic"] == "observations/state") {
                    int new_time_step =
                        T - this->get_remaining_seconds_from(
                                message["data"]["mission_timer"]);

                    if (new_time_step == this->time_step + this->time_gap) {
                        // After the first time step, 0 represents not observing
                        // an event.
                        if (this->last_observations_per_node[ROOM] == NO_OBS) {
                            this->last_observations_per_node[ROOM] = 0;
                        }
                        if (this->last_observations_per_node[SG] == NO_OBS) {
                            this->last_observations_per_node[SG] = 0;
                        }
                        if (this->last_observations_per_node[SY] == NO_OBS) {
                            this->last_observations_per_node[SY] = 0;
                        }
                        if (this->last_observations_per_node[BEEP] == NO_OBS) {
                            this->last_observations_per_node[BEEP] = 0;
                        }

                        observations_per_node =
                            this->last_observations_per_node;

                        // The following nodes are events and should have value
                        // only if an explicit message is received. Others will
                        // preserve the values of the previous observation until
                        // a new message comes to change that.
                        this->last_observations_per_node[BEEP] = 0;
                        this->time_step = new_time_step;
                    }
                }
                else if (message["topic"] ==
                         "observations/events/player/triage") {
                    this->fill_victim_saving_observation(message);
                }
                else if (message["topic"] ==
                         "observations/events/player/location") {
                    this->fill_room_observation(message);
                }
                else if (message["topic"] ==
                         "observations/events/player/beep") {
                    this->fill_beep_observation(message);
                }
            }

            return observations_per_node;
        }

        int
        TA3MessageConverter::get_remaining_seconds_from(const string& time) {
            int minutes = 0;
            int seconds = 0;

            try {
                minutes = stoi(time.substr(0, time.find(":")));
                seconds = stoi(time.substr(time.find(":") + 1, time.size()));
            }
            catch (std::invalid_argument& e) {
            }

            return seconds + minutes * 60;
        }

        void TA3MessageConverter::fill_victim_saving_observation(
            const nlohmann::json& json_message) {

            string node_label;
            double value = 0;

            if (json_message["data"]["triage_state"] == "IN_PROGRESS") {
                value = 1;
            }

            if (json_message["data"]["color"] == "Green") {
                node_label = SG;
            }
            else if (json_message["data"]["color"] == "Yellow") {
                node_label = SY;
            }

            this->last_observations_per_node[node_label] = value;
        }

        void TA3MessageConverter::fill_room_observation(
            const nlohmann::json& json_message) {

            double value = 0;
            if (json_message["data"].contains("locations")) {
                string room_id = json_message["data"]["locations"][0]["id"];
                if (EXISTS(room_id, this->map_area_configuration)) {
                    if (this->map_area_configuration.at(room_id)) {
                        value = 1;
                    }

                    this->last_observations_per_node[ROOM] = value;
                }
            }
        }

        void TA3MessageConverter::fill_training_condition_observation(
            const nlohmann::json& json_message) {

            const string value = json_message["data"]["condition"];
            this->last_observations_per_node[Q] = stoi(value);
        }

        void TA3MessageConverter::fill_beep_observation(
            const nlohmann::json& json_message) {

            const string beep = json_message["data"]["message"];
            int value = 1; // Beep. 0 is reserved for not observing a beep.
            if (beep == "Beep Beep") {
                value = 2;
            }

            this->last_observations_per_node[BEEP] = value;
        }

    } // namespace model
} // namespace tomcat

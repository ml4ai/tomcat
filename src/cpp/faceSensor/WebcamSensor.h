#pragma once

#include <SequenceCapture.h>
#include <VisualizationUtils.h>
#include <Visualizer.h>
#include <nlohmann/json.hpp>

namespace tomcat {

    class WebcamSensor {
      public:
        WebcamSensor()
            : visualizer(true, false, false, false),
              det_parameters(this->arguments) {}

        void initialize(std::string exp,
                        std::string trial,
                        std::string pname,
                        bool ind,
                        bool vis,
                        std::string file_path,
                        bool emo);
        void get_observation();

      private:
        std::vector<std::string> arguments = {"-au_static"};
        Utilities::Visualizer visualizer;
        cv::Mat rgb_image;
        Utilities::SequenceCapture sequence_reader;
        LandmarkDetector::CLNF face_model;
        LandmarkDetector::FaceModelParameters det_parameters;
        Utilities::FpsTracker fps_tracker;
        cv::Mat_<uchar> grayscale_image;
        std::string exp_id;
        std::string trial_id;
        std::string playername;
        bool indent;
        bool visual;
        bool emotion;
        std::string get_emotion(nlohmann::json emotion);
    };

} // namespace tomcat

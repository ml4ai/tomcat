#pragma once

#include <fmt/format.h>

#include <LandmarkCoreIncludes.h>
#include <SequenceCapture.h>
#include <VisualizationUtils.h>
#include <Visualizer.h>
#include <GazeEstimation.h>

namespace tomcat {

    class WebcamSensor {
      public:
        WebcamSensor()
            : visualizer(true, false, false, false),
              det_parameters(this->arguments) {}

        void initialize() {
            // The modules that are being used for tracking
            this->face_model = LandmarkDetector::CLNF();
            if (!this->face_model.loaded_successfully) {
                throw std::runtime_error(
                    "Could not load the landmark detector.");
            }

            if (!this->face_model.eye_model) {
                throw std::runtime_error("No eye model found");
            }

            this->fps_tracker.AddFrame();
            this->sequence_reader.Open(arguments);
            this->rgb_image = this->sequence_reader.GetNextFrame();
        };

        void get_observation();

      private:
        std::vector<std::string> arguments = {"-device", "0"};
        Utilities::Visualizer visualizer;
        cv::Mat rgb_image;
        Utilities::SequenceCapture sequence_reader;
        LandmarkDetector::CLNF face_model;
        LandmarkDetector::FaceModelParameters det_parameters;
        Utilities::FpsTracker fps_tracker;
        cv::Mat_<uchar> grayscale_image;
    };


    void get_observation();

  private:
    std::vector<std::string> arguments = {"-device", "0","-au_static"};
    Utilities::Visualizer visualizer;
    cv::Mat rgb_image;
    Utilities::SequenceCapture sequence_reader;
    LandmarkDetector::CLNF face_model;
    LandmarkDetector::FaceModelParameters det_parameters;
    Utilities::FpsTracker fps_tracker;
    cv::Mat_<uchar> grayscale_image;
  };

} // namespace tomcat

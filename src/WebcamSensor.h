#pragma once

#include <fmt/core.h>

#include <LandmarkCoreIncludes.h>
#include <SequenceCapture.h>
#include <VisualizationUtils.h>
#include <Visualizer.h>

namespace tomcat {

  class WebcamSensor {
  public:
    WebcamSensor()
        : visualizer(true, false, false, false), det_parameters(arguments) {}

    void initialize() {
      // The modules that are being used for tracking
      this->face_model = LandmarkDetector::CLNF();
      if (!face_model.loaded_successfully) {
        fmt::print("ERROR: Could not load the landmark detector");
      }

      if (!face_model.eye_model) {
        fmt::print("WARNING: no eye model found");
      }

      this->fps_tracker.AddFrame();
      this->sequence_reader.Open(arguments);
      fmt::print("Device or file opened");
      fmt::print("Starting tracking");
      this->rgb_image = this->sequence_reader.GetNextFrame();
    };

    void get_observation();

  private:
    vector<string> arguments = {"-device", "0"};
    Utilities::Visualizer visualizer;
    cv::Mat rgb_image;
    Utilities::SequenceCapture sequence_reader;
    LandmarkDetector::CLNF face_model;
    LandmarkDetector::FaceModelParameters det_parameters;
    Utilities::FpsTracker fps_tracker;
    cv::Mat_<uchar> grayscale_image;
  };

} // namespace tomcat

#include <fmt/format.h>

#include "WebcamSensor.h"
#include <GazeEstimation.h>
#include <opencv2/highgui/highgui.hpp>
#include<opencv2/videoio.hpp>

namespace tomcat {

  void WebcamSensor::get_observation() {
    using cv::Point3f;
    using cv::Vec6d;
    using GazeAnalysis::EstimateGaze;
    using LandmarkDetector::Calculate3DEyeLandmarks;
    using LandmarkDetector::CalculateAllEyeLandmarks;
    using LandmarkDetector::DetectLandmarksInVideo;
    using LandmarkDetector::FaceModelParameters;
    using LandmarkDetector::GetPose;

    // Reading the images
    this->grayscale_image = this->sequence_reader.GetGrayFrame();

    // The actual facial landmark detection / tracking
    bool detection_success = DetectLandmarksInVideo(this->rgb_image,
                                                    this->face_model,
                                                    this->det_parameters,
                                                    this->grayscale_image);

    // Gaze tracking, absolute gaze direction
    Point3f gazeDirection0(0, 0, -1);
    Point3f gazeDirection1(0, 0, -1);

    // If tracking succeeded and we have an eye model, estimate gaze
    if (detection_success && this->face_model.eye_model) {

      EstimateGaze(this->face_model,
                   gazeDirection0,
                   this->sequence_reader.fx,
                   this->sequence_reader.fy,
                   this->sequence_reader.cx,
                   this->sequence_reader.cy,
                   true);

      EstimateGaze(this->face_model,
                   gazeDirection1,
                   this->sequence_reader.fx,
                   this->sequence_reader.fy,
                   this->sequence_reader.cx,
                   this->sequence_reader.cy,
                   false);
    }

    // Work out the pose of the head from the tracked model
    Vec6d pose_estimate = GetPose(this->face_model,
                                  this->sequence_reader.fx,
                                  this->sequence_reader.fy,
                                  this->sequence_reader.cx,
                                  this->sequence_reader.cy);

    this->fps_tracker.AddFrame();
    // Displaying the tracking visualizations
    this->visualizer.SetImage(this->rgb_image,
                              this->sequence_reader.fx,
                              this->sequence_reader.fy,
                              this->sequence_reader.cx,
                              this->sequence_reader.cy);

    this->visualizer.SetObservationLandmarks(
        this->face_model.detected_landmarks,
        this->face_model.detection_certainty,
        this->face_model.GetVisibilities());

    this->visualizer.SetObservationPose(pose_estimate,
                                        this->face_model.detection_certainty);

    this->visualizer.SetObservationGaze(
        gazeDirection0,
        gazeDirection1,
        CalculateAllEyeLandmarks(this->face_model),
        Calculate3DEyeLandmarks(this->face_model,
                                this->sequence_reader.fx,
                                this->sequence_reader.fy,
                                this->sequence_reader.cx,
                                this->sequence_reader.cy),
        this->face_model.detection_certainty);

    this->visualizer.SetFps(this->fps_tracker.GetFPS());

    char character_press = this->visualizer.ShowObservation();
    if (character_press == 'r') {
      this->face_model.Reset();
    }

    this->rgb_image = this->sequence_reader.GetNextFrame();
  }
    VideoCapture capture(0);
    VideoWriter writer("VideoTest.avi", CV_FOURCC('M', 'J', 'P', 'G'), 25.0, Size(640, 480));
    Mat frame;

    while (capture.isOpened())
    {
       capture >> frame;
       writer << frame;
       imshow("video", frame);
       if (cvWaitKey(20) == 27)
       {
           break;
       }
     }



} // namespace tomcat

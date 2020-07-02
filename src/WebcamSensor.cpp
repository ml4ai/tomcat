#include "WebcamSensor.h"
#include <FaceAnalyser.h>
#include <GazeEstimation.h>
#include <LandmarkCoreIncludes.h>
#include <RecorderOpenFace.h>
#include <nlohmann/json.hpp>
#include <opencv2/highgui/highgui.hpp>

using json = nlohmann::json;
using namespace std;

namespace tomcat {

    void WebcamSensor::initialize() {
        // The modules that are being used for tracking
        this->face_model = LandmarkDetector::CLNF();
        if (!this->face_model.loaded_successfully) {
            throw runtime_error("Could not load the landmark detector.");
        }

        if (!this->face_model.eye_model) {
            throw runtime_error("No eye model found");
        }

        this->fps_tracker.AddFrame();
        this->sequence_reader.Open(arguments);
        this->rgb_image = this->sequence_reader.GetNextFrame();
    };

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

        //.face analysis
        FaceAnalysis::FaceAnalyserParameters face_analysis_params(
            this->arguments);
        FaceAnalysis::FaceAnalyser face_analyser(face_analysis_params);

        //.RecorderOpenFaceParameters
        Utilities::RecorderOpenFaceParameters recording_params(
            this->arguments,
            true,
            this->sequence_reader.IsWebcam(),
            this->sequence_reader.fx,
            this->sequence_reader.fy,
            this->sequence_reader.cx,
            this->sequence_reader.cy,
            this->sequence_reader.fps);

        // .Create open_face_rec
        Utilities::RecorderOpenFace open_face_rec(
            this->sequence_reader.name, recording_params, this->arguments);

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

        // .Do face alignment
        cv::Mat sim_warped_img;
        cv::Mat_<double> hog_descriptor;
        int num_hog_rows = 0, num_hog_cols = 0;

        // .Perform AU detection and HOG feature extraction, as this can be
        // expensive only compute it if needed by output or visualization
        if (recording_params.outputAlignedFaces() ||
            recording_params.outputHOG() || recording_params.outputAUs() ||
            this->visualizer.vis_align || this->visualizer.vis_hog ||
            this->visualizer.vis_aus) {
            face_analyser.AddNextFrame(this->rgb_image,
                                       this->face_model.detected_landmarks,
                                       this->face_model.detection_success,
                                       this->sequence_reader.time_stamp,
                                       this->sequence_reader.IsWebcam());
            face_analyser.GetLatestAlignedFace(sim_warped_img);
            face_analyser.GetLatestHOG(
                hog_descriptor, num_hog_rows, num_hog_cols);
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

        //.
        this->visualizer.SetObservationFaceAlign(sim_warped_img);
        this->visualizer.SetObservationHOG(
            hog_descriptor, num_hog_rows, num_hog_cols);

        this->visualizer.SetObservationLandmarks(
            this->face_model.detected_landmarks,
            this->face_model.detection_certainty,
            this->face_model.GetVisibilities());

        this->visualizer.SetObservationPose(
            pose_estimate, this->face_model.detection_certainty);

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

        this->visualizer.SetObservationActionUnits(
            face_analyser.GetCurrentAUsReg(),
            face_analyser.GetCurrentAUsClass());

        this->visualizer.SetFps(this->fps_tracker.GetFPS());

        char character_press = this->visualizer.ShowObservation();
        if (character_press == 'r') {
            this->face_model.Reset();
        }

        // Setting up the recorder output
        open_face_rec.SetObservationHOG(
            detection_success,
            hog_descriptor,
            num_hog_rows,
            num_hog_cols,
            31); // The number of channels in HOG is fixed at the moment, as
                 // using FHOG
        open_face_rec.SetObservationVisualization(
            this->visualizer.GetVisImage());
        open_face_rec.SetObservationActionUnits(
            face_analyser.GetCurrentAUsReg(),
            face_analyser.GetCurrentAUsClass());

        this->rgb_image = this->sequence_reader.GetNextFrame();

        cout << "Postprocessing the Action Unit predictions" << endl;
        json j;
        j = {{"au_intensities", open_face_rec.get_au_intensities()},
             {"au_occurences", open_face_rec.get_au_occurences()}};

        //. Reset the models for the next video
        face_analyser.Reset();
        face_model.Reset();
    }

} // namespace tomcat

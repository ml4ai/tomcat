#include "WebcamSensor.h"
#include <FaceAnalyser.h>
#include <GazeEstimation.h>
#include <LandmarkCoreIncludes.h>
#include <RecorderOpenFace.h>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/date_time/posix_time/posix_time_duration.hpp>
#include <boost/date_time/time_facet.hpp>
#include <nlohmann/json.hpp>
#include <opencv2/highgui/highgui.hpp>

using namespace std;
using namespace nlohmann;

typedef vector<pair<string, double>> au_vector;

namespace tomcat {

    void
    WebcamSensor::initialize(string exp, string trial, string pname, bool ind) {
        // Initialize the experiment ID, trial ID and player name
        this->exp_id = exp;
        this->trial_id = trial;
        this->playername = pname;
        this->indent = ind;

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
    };

    void WebcamSensor::get_observation() {
        using cv::Point2f;
        using cv::Point3f;
        using GazeAnalysis::EstimateGaze;
        using LandmarkDetector::Calculate3DEyeLandmarks;
        using LandmarkDetector::CalculateAllEyeLandmarks;
        using LandmarkDetector::DetectLandmarksInVideo;
        using LandmarkDetector::FaceModelParameters;
        using LandmarkDetector::GetPose;

        // Start time for facesensor
        boost::posix_time::ptime t_start(
            boost::posix_time::microsec_clock::universal_time());

        // Load facial feature extractor and AU analyser
        FaceAnalysis::FaceAnalyserParameters face_analysis_params(
            this->arguments);
        FaceAnalysis::FaceAnalyser face_analyser(face_analysis_params);

        // Recorder open face parameters
        Utilities::RecorderOpenFaceParameters recording_params(
            this->arguments,
            true,
            this->sequence_reader.IsWebcam(),
            this->sequence_reader.fx,
            this->sequence_reader.fy,
            this->sequence_reader.cx,
            this->sequence_reader.cy,
            this->sequence_reader.fps);

        this->rgb_image = this->sequence_reader.GetNextFrame();

        while (!this->rgb_image.empty()) {

            // Converting to grayscale
            this->grayscale_image = this->sequence_reader.GetGrayFrame();

            // The actual facial landmark detection / tracking
            bool detection_success =
                DetectLandmarksInVideo(this->rgb_image,
                                       this->face_model,
                                       this->det_parameters,
                                       this->grayscale_image);

            // Gaze tracking, absolute gaze direction
            Point3f gazeDirection0(0, 0, 0);
            Point3f gazeDirection1(0, 0, 0);
            cv::Vec2d gazeAngle(0, 0);

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

                gazeAngle =
                    GazeAnalysis::GetGazeAngle(gazeDirection0, gazeDirection1);
            }

            // Do face alignment
            cv::Mat sim_warped_img;
            cv::Mat_<double> hog_descriptor;
            int num_hog_rows = 0, num_hog_cols = 0;

            // Perform AU detection and HOG feature extraction
            // Note: As this can be expensive, only compute it if needed by
            // output or visualization
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
            cv::Vec6d pose_estimate = GetPose(this->face_model,
                                              this->sequence_reader.fx,
                                              this->sequence_reader.fy,
                                              this->sequence_reader.cx,
                                              this->sequence_reader.cy);

            // Keeping track of fps
            this->fps_tracker.AddFrame();

            // Displaying the tracking visualizations
            this->visualizer.SetImage(this->rgb_image,
                                      this->sequence_reader.fx,
                                      this->sequence_reader.fy,
                                      this->sequence_reader.cx,
                                      this->sequence_reader.cy);

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

            // Detect key press
            char character_press = this->visualizer.ShowObservation();
            if (character_press == 'r') {
                this->face_model.Reset();
            }
            else if (character_press == 'q') {
                break;
            }

            // JSON output
            json output;

            int micro_timestamp =
                int(this->sequence_reader.time_stamp * pow(10, 6));
            t_start += boost::posix_time::microseconds(micro_timestamp);
            string str_timestamp = to_iso_extended_string(t_start);
            str_timestamp.push_back('Z');

            // Header block
            output["header"] = {
                {"timestamp", str_timestamp},
                {"message_type", "observation"},
                {"version", "0.1"},
            };

            // Message block
            output["msg"] = {{"experiment_id", this->exp_id},
                             {"trial_id", this->trial_id},
                             {"timestamp", str_timestamp},
                             {"source", "facesensor"},
                             {"sub_type", "state"},
                             {"version", "0.1"}};

            // Data block
            stringstream ss;
            ss << fixed << setprecision(5)
               << this->face_model.detection_certainty;
            output["data"] = {
                {"playername", this->playername},
                {"landmark_detection_confidence", ss.str()},
                {"landmark_detection_success", detection_success},
                {"frame", this->sequence_reader.GetFrameNumber()}};

            au_vector AU_reg = face_analyser.GetCurrentAUsReg();
            au_vector AU_class = face_analyser.GetCurrentAUsClass();
            sort(AU_reg.begin(), AU_reg.end());
            sort(AU_class.begin(), AU_class.end());
            for (auto& [AU, occurrence] : AU_class) {
                output["data"]["action_units"][AU]["occurrence"] = occurrence;
            }

            for (auto& [AU, intensity] : AU_reg) {
                output["data"]["action_units"][AU]["intensity"] = intensity;
            }

            output["data"]["gaze"] = {
                {"eye_0",
                 {
                     {"x", gazeDirection0.x},
                     {"y", gazeDirection0.y},
                     {"z", gazeDirection0.z},
                 }},
                {"eye_1",
                 {
                     {"x", gazeDirection1.x},
                     {"y", gazeDirection1.y},
                     {"z", gazeDirection1.z},
                 }},
                {"gaze_angle", {{"x", gazeAngle[0]}, {"y", gazeAngle[1]}}}

            };

            vector<Point2f> eye_landmarks2d =
                CalculateAllEyeLandmarks(this->face_model);
            vector<Point3f> eye_landmarks3d =
                Calculate3DEyeLandmarks(this->face_model,
                                        this->sequence_reader.fx,
                                        this->sequence_reader.fy,
                                        this->sequence_reader.cx,
                                        this->sequence_reader.cy);
            for (int i = 0; i < eye_landmarks2d.size(); i++) {
                string x = "x_";
                ostringstream ostr;
                ostr << i;
                x.append(ostr.str());
                output["data"]["gaze"]["eye_landmarks"]["2D"][x] =
                    eye_landmarks2d[i].x;
            }
            for (int i = 0; i < eye_landmarks2d.size(); i++) {
                string y = "y_";
                ostringstream ostr;
                ostr << i;
                y.append(ostr.str());
                output["data"]["gaze"]["eye_landmarks"]["2D"][y] =
                    eye_landmarks2d[i].y;
            }
            for (int i = 0; i < eye_landmarks3d.size(); i++) {
                string x = "X_";
                ostringstream ostr;
                ostr << i;
                x.append(ostr.str());
                output["data"]["gaze"]["eye_landmarks"]["3D"][x] =
                    eye_landmarks3d[i].x;
            }
            for (int i = 0; i < eye_landmarks3d.size(); i++) {
                string y = "Y_";
                ostringstream ostr;
                ostr << i;
                y.append(ostr.str());
                output["data"]["gaze"]["eye_landmarks"]["3D"][y] =
                    eye_landmarks3d[i].y;
            }
            for (int i = 0; i < eye_landmarks3d.size(); i++) {
                string z = "Z_";
                ostringstream ostr;
                ostr << i;
                z.append(ostr.str());
                output["data"]["gaze"]["eye_landmarks"]["3D"][z] =
                    eye_landmarks3d[i].z;
            }

            output["data"]["pose"] = {{"location",
                                       {{"x", pose_estimate[0]},
                                        {"y", pose_estimate[1]},
                                        {"z", pose_estimate[2]}}},
                                      {"rotation",
                                       {{"x", pose_estimate[3]},
                                        {"y", pose_estimate[4]},
                                        {"z", pose_estimate[5]}}}};

            // Only indent if the user specifies through command line option
            // --indent
            if (this->indent)
                cout << output.dump(4) << endl;
            else
                cout << output.dump() << endl;

            // Grabbing the next frame in the sequence
            this->rgb_image = this->sequence_reader.GetNextFrame();
        }

        this->sequence_reader.Close();

        // Reset the models for the next video
        face_analyser.Reset();
        face_model.Reset();
    }

} // namespace tomcat

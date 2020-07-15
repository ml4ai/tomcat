#include "WebcamSensor.h"
#include <FaceAnalyser.h>
#include <GazeEstimation.h>
#include <LandmarkCoreIncludes.h>
#include <RecorderOpenFace.h>
#include <opencv2/highgui/highgui.hpp>
#include <nlohmann/json.hpp>
#include "include/nlohmann/fifo_map.hpp"

typedef vector<pair<string, double>> au_vector;


using namespace std;
using namespace nlohmann;


template<class K, class V, class dummy_compare, class A>
using modified_fifo_map = fifo_map<K, V, fifo_map_compare<K>, A>;
using modified_json = basic_json<modified_fifo_map>;

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
    };

    void WebcamSensor::get_observation() {
        using cv::Point3f;
        using cv::Vec6d;
        using cv::Vec2d;
        using GazeAnalysis::EstimateGaze;
        using GazeAnalysis::GetGazeAngle;
        using LandmarkDetector::Calculate3DEyeLandmarks;
        using LandmarkDetector::CalculateAllEyeLandmarks;
        using LandmarkDetector::DetectLandmarksInVideo;
        using LandmarkDetector::FaceModelParameters;
        using LandmarkDetector::GetPose;
        
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

        // Create open_face_rec
        Utilities::RecorderOpenFace open_face_rec(
            this->sequence_reader.name, recording_params, this->arguments);
            
            
		this->rgb_image = this->sequence_reader.GetNextFrame(); 
		
		while (!this->rgb_image.empty()) {
		            
            // Converting to grayscale
        	this->grayscale_image = this->sequence_reader.GetGrayFrame();

         	// The actual facial landmark detection / tracking
        	bool detection_success = DetectLandmarksInVideo(this->rgb_image,
            	                                            this->face_model,
            	                                            this->det_parameters,
            	                                            this->grayscale_image);

        	// Gaze tracking, absolute gaze direction
        	Point3f gazeDirection0(0, 0, 0);
        	Point3f gazeDirection1(0, 0, 0);
        	Vec2d gazeAngle(0, 0);
        	

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
                	         
               gazeAngle = GetGazeAngle(gazeDirection0, gazeDirection1);
        	}
        
        
	        // Do face alignment
	        cv::Mat sim_warped_img;
	        cv::Mat_<double> hog_descriptor;
	        int num_hog_rows = 0, num_hog_cols = 0;

        	// Perform AU detection and HOG feature extraction, as this can be
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
        	
        	open_face_rec.SetObservationLandmarks(
        		this->face_model.detected_landmarks,
        		this->face_model.GetShape(
        			this->sequence_reader.fx, 
        			this->sequence_reader.fy, 
        			this->sequence_reader.cx, 
        			this->sequence_reader.cy),
				this->face_model.params_global, 
				this->face_model.params_local, 
				this->face_model.detection_certainty, 
				detection_success);
        	
        	open_face_rec.SetObservationTimestamp(
        		this->sequence_reader.time_stamp);
        	
        	open_face_rec.SetObservationFrameNumber(
        		this->sequence_reader.GetFrameNumber());
        	
        	open_face_rec.SetObservationActionUnits(
        	    face_analyser.GetCurrentAUsReg(),
        	    face_analyser.GetCurrentAUsClass());
        	
        	open_face_rec.SetObservationGaze(
        		gazeDirection0, 
        		gazeDirection1, 
        		gazeAngle, 
        		CalculateAllEyeLandmarks(this->face_model),
        		Calculate3DEyeLandmarks(
        				this->face_model,
        				this->sequence_reader.fx, 
        				this->sequence_reader.fy, 
        				this->sequence_reader.cx, 
        				this->sequence_reader.cy));
        	        	
        	// Json output		
        	modified_json output;
        	
        	// Header block
        	output["header"]["timestamp"] = this->sequence_reader.time_stamp;
        	output["header"]["message_type"] = "observation";
        	output["header"]["version"] = "0.1";
        	
        	// Message block
        	output["msg"]["timestamp"] = this->sequence_reader.time_stamp;
        	output["msg"]["source"] = "facesensor";
        	output["msg"]["sub_type"] = "state";
        	output["msg"]["version"] = "0.1";
        	
        	// Data block
        	std::stringstream ss;
			ss << std::fixed << std::setprecision(5) << this->face_model.detection_certainty;
        	output["data"]["landmark_detection_confidence"] = ss.str();
        	output["data"]["landmark_detection_success"] = detection_success;
        	output["data"]["frame"] = this->sequence_reader.GetFrameNumber();
        	
        	vector<pair<string, double>> AU_reg, AU_class;
        	AU_reg = face_analyser.GetCurrentAUsReg();
        	AU_class = 	face_analyser.GetCurrentAUsClass();
        	sort(AU_reg.begin(), AU_reg.end()); 
        	sort(AU_class.begin(), AU_class.end()); 
        	au_vector::iterator it_reg, it_class;
        	for (it_class = AU_class.begin(); it_class != AU_class.end(); ++it_class) {
        		output["data"]["action_units"][it_class->first]["occurence"] = it_class->second;
        	}
        	for (it_reg = AU_reg.begin(); it_reg != AU_reg.end(); ++it_reg) {
        		output["data"]["action_units"][it_reg->first]["intensity"] = it_reg->second;
        	}
        	
        	output["data"]["gaze"]["eye_0"]["x"] = gazeDirection0.x;
        	output["data"]["gaze"]["eye_0"]["y"] = gazeDirection0.y;
        	output["data"]["gaze"]["eye_0"]["z"] = gazeDirection0.z;
        	output["data"]["gaze"]["eye_1"]["x"] = gazeDirection1.x;
        	output["data"]["gaze"]["eye_1"]["y"] = gazeDirection1.y;
        	output["data"]["gaze"]["eye_1"]["z"] = gazeDirection1.z;
        	output["data"]["gaze"]["gaze_angle"]["x"] = gazeAngle[0];
        	output["data"]["gaze"]["gaze_angle"]["y"] = gazeAngle[1];

        	
        	std::cout << output.dump(4) << std::endl;
        		
        	// Grabbing the next frame in the sequence
        	this->rgb_image = this->sequence_reader.GetNextFrame();
        	
		}

        
        open_face_rec.Close();
        this->sequence_reader.Close();

        // Reset the models for the next video
        face_analyser.Reset();
        face_model.Reset();
    }

} // namespace tomcat

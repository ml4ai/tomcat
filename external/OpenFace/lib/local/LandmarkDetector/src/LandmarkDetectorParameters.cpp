///////////////////////////////////////////////////////////////////////////////
// Copyright (C) 2017, Carnegie Mellon University and University of Cambridge,
// all rights reserved.
//
// ACADEMIC OR NON-PROFIT ORGANIZATION NONCOMMERCIAL RESEARCH USE ONLY
//
// BY USING OR DOWNLOADING THE SOFTWARE, YOU ARE AGREEING TO THE TERMS OF THIS
// LICENSE AGREEMENT. IF YOU DO NOT AGREE WITH THESE TERMS, YOU MAY NOT USE OR
// DOWNLOAD THE SOFTWARE.
//
// License can be found in OpenFace-license.txt
//
//     * Any publications arising from the use of this software, including but
//       not limited to academic journal and conference publications, technical
//       reports and manuals, must cite at least one of the following works:
//
//       OpenFace 2.0: Facial Behavior Analysis Toolkit
//       Tadas Baltrušaitis, Amir Zadeh, Yao Chong Lim, and Louis-Philippe
//       Morency in IEEE International Conference on Automatic Face and Gesture
//       Recognition, 2018
//
//       Convolutional experts constrained local model for facial landmark
//       detection. A. Zadeh, T. Baltrušaitis, and Louis-Philippe Morency, in
//       Computer Vision and Pattern Recognition Workshops, 2017.
//
//       Rendering of Eyes for Eye-Shape Registration and Gaze Estimation
//       Erroll Wood, Tadas Baltrušaitis, Xucong Zhang, Yusuke Sugano, Peter
//       Robinson, and Andreas Bulling in IEEE International. Conference on
//       Computer Vision (ICCV),  2015
//
//       Cross-dataset learning and person-specific normalisation for automatic
//       Action Unit detection Tadas Baltrušaitis, Marwa Mahmoud, and Peter
//       Robinson in Facial Expression Recognition and Analysis Challenge, IEEE
//       International Conference on Automatic Face and Gesture Recognition,
//       2015
//
///////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "LandmarkDetectorParameters.h"

// Boost includes
#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>

// System includes
#include <cstdlib>
#include <iostream>
#include <sstream>

using namespace std;
using boost::filesystem::exists;
using boost::filesystem::path;

namespace LandmarkDetector {

  void FaceModelParameters::check_model_path(string model_path) {
    if (!exists(path(model_path))) {
      throw runtime_error("Could not find the the model " + model_path);
    }
  }

  void FaceModelParameters::check_model_paths() {
    this->check_model_path(this->model_location);
    this->check_model_path(this->haar_face_detector_location);
    this->check_model_path(this->mtcnn_face_detector_location);
  }

  FaceModelParameters::FaceModelParameters() {
    // initialise the default values
    this->init();
    this->check_model_paths();
  }

  FaceModelParameters::FaceModelParameters(vector<string>& arguments) {
    // initialise the default values
    this->init();

    // First element is reserved for the executable location (useful for finding
    // relative model locs)
    path root = path(arguments[0]).parent_path();

    bool* valid = new bool[arguments.size()];
    valid[0] = true;

    for (size_t i = 1; i < arguments.size(); ++i) {
      valid[i] = true;

      if (arguments[i].compare("-mloc") == 0) {
        string model_loc = arguments[i + 1];
        this->model_location = model_loc;
        valid[i] = false;
        valid[i + 1] = false;
        i++;
      }
      if (arguments[i].compare("-fdloc") == 0) {
        string face_detector_loc = arguments[i + 1];
        this->haar_face_detector_location = face_detector_loc;
        this->curr_face_detector = HAAR_DETECTOR;
        valid[i] = false;
        valid[i + 1] = false;
        i++;
      }
      if (arguments[i].compare("-sigma") == 0) {
        stringstream data(arguments[i + 1]);
        data >> sigma;
        valid[i] = false;
        valid[i + 1] = false;
        i++;
      }
      else if (arguments[i].compare("-w_reg") == 0) {
        stringstream data(arguments[i + 1]);
        data >> this->weight_factor;
        valid[i] = false;
        valid[i + 1] = false;
        i++;
      }
      else if (arguments[i].compare("-reg") == 0) {
        stringstream data(arguments[i + 1]);
        data >> this->reg_factor;
        valid[i] = false;
        valid[i + 1] = false;
        i++;
      }
      else if (arguments[i].compare("-multi_view") == 0) {

        stringstream data(arguments[i + 1]);
        int m_view;
        data >> m_view;

        this->multi_view = (bool)(m_view != 0);
        valid[i] = false;
        valid[i + 1] = false;
        i++;
      }
      else if (arguments[i].compare("-validate_detections") == 0) {
        stringstream data(arguments[i + 1]);
        int v_det;
        data >> v_det;

        this->validate_detections = (bool)(v_det != 0);
        valid[i] = false;
        valid[i + 1] = false;
        i++;
      }
      else if (arguments[i].compare("-n_iter") == 0) {
        stringstream data(arguments[i + 1]);
        data >> this->num_optimisation_iteration;

        valid[i] = false;
        valid[i + 1] = false;
        i++;
      }
      else if (arguments[i].compare("-wild") == 0) {
        // For in the wild fitting these parameters are suitable
        this->window_sizes_init = vector<int>(4);
        this->window_sizes_init[0] = 15;
        this->window_sizes_init[1] = 13;
        this->window_sizes_init[2] = 11;
        this->window_sizes_init[3] = 11;

        this->sigma = 1.25;
        this->reg_factor = 35;
        this->weight_factor = 2.5;
        this->num_optimisation_iteration = 10;

        valid[i] = false;

        // For in-the-wild images use an in-the wild detector
        this->curr_face_detector = MTCNN_DETECTOR;

        // Use multi-view hypotheses if in-the-wild setting
        this->multi_view = true;
      }
    }

    for (int i = (int)arguments.size() - 1; i >= 0; --i) {
      if (!valid[i]) {
        arguments.erase(arguments.begin() + i);
      }
    }

    this->check_model_paths();
    path model_path = path(this->model_location);

    if (model_path.stem().string().compare("main_ceclm_general") == 0) {
      this->curr_landmark_detector = this->CECLM_DETECTOR;
      this->sigma = 1.5f * sigma;
      this->reg_factor = 0.9f * reg_factor;
    }
    else if (model_path.stem().string().compare("main_clnf_general") == 0) {
      this->curr_landmark_detector = this->CLNF_DETECTOR;
    }
    else if (model_path.stem().string().compare("main_clm_general") == 0) {
      this->curr_landmark_detector = this->CLM_DETECTOR;
    }
  }

  void FaceModelParameters::init() {

    // number of iterations that will be performed at each scale
    this->num_optimisation_iteration = 5;

    // using an external face checker based on SVM
    this->validate_detections = true;

    // Using hierarchical refinement by default (can be turned off)
    this->refine_hierarchical = true;

    // Refining parameters by default
    this->refine_parameters = true;

    // For fast tracking
    this->window_sizes_small = {0, 9, 7, 0};

    // Just for initialisation
    this->window_sizes_init = {11, 9, 7, 5};

    this->face_template_scale = 0.3f;
    // Off by default (as it might lead to some slight inaccuracies in slowly
    // moving faces)
    use_face_template = false;

    // For first frame use the initialisation
    this->window_sizes_current = window_sizes_init;

    this->model_location = this->OpenFace_models_dir +
                           "/landmark_detection/main_ceclm_general.txt";
    this->curr_landmark_detector = CECLM_DETECTOR;

    this->sigma = 1.5f;
    this->reg_factor = 25.0f;
    this->weight_factor = 0.0f; // By default do not use NU-RLMS for videos as
                                // it does not work as well for them

    this->validation_boundary = 0.725f;

    this->limit_pose = true;
    this->multi_view = false;

    this->reinit_video_every = 2;

    // Face detection
    this->haar_face_detector_location =
        this->OpenFace_models_dir +
        "/classifiers/haarcascade_frontalface_alt.xml";
    this->mtcnn_face_detector_location =
        this->OpenFace_models_dir +
        "/landmark_detection/mtcnn_detector/MTCNN_detector.txt";

    // By default use MTCNN
    this->curr_face_detector = MTCNN_DETECTOR;
  }

} // namespace LandmarkDetector

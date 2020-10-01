cmake_minimum_required(VERSION 3.10)

project(model)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# External dependencies

# fmt is an open-source C++ formatting library
find_package(FMT REQUIRED)

# Eigen is a C++ library used for matrix and vector operations.
find_package(Eigen3 REQUIRED NO_MODULE)

# GSL is a C library used for sampling from a determined distribution.
find_package(GSL REQUIRED)

# Boost is used for several requirements in the project.
find_package(
  Boost 1.69
  COMPONENTS chrono
             date_time
             filesystem
             iostreams
             program_options
             regex
             system
             thread
             REQUIRED)

# FMT is a library used for formatting.
find_package(FMT REQUIRED)

# Json library is being used for writing evaluations to an output stream
find_package(nlohmann_json REQUIRED)

# Finding the Mosquitto header and library
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH}
                      "${CMAKE_SOURCE_DIR}/../../../tools/cmake")
find_package(Mosquitto 1.5 REQUIRED)

# Header files inside model folder
file(GLOB tomcat_model_headers CONFIGURE_DEPENDS
     ${CMAKE_CURRENT_SOURCE_DIR}/*.h)

# Header and cpp files
add_library(
  tomcat_model_lib STATIC
  ${tomcat_model_headers}
  pgm/NodeMetadata.cpp
  pgm/Node.cpp
  pgm/ConstantNode.cpp
  pgm/RandomVariableNode.cpp
  pgm/cpd/CPD.cpp
  pgm/cpd/CategoricalCPD.cpp
  pgm/cpd/GaussianCPD.cpp
  pgm/cpd/DirichletCPD.cpp
  pgm/DynamicBayesNet.cpp
  sampling/Sampler.cpp
  sampling/AncestralSampler.cpp
  sampling/GibbsSampler.cpp
  utils/FileHandler.cpp
  utils/Tensor3.cpp
  distribution/Distribution.cpp
  distribution/Categorical.cpp
  distribution/Continuous.cpp
  distribution/Dirichlet.cpp
  distribution/Gaussian.cpp
  pipeline/training/DBNSamplingTrainer.cpp
  pgm/EvidenceSet.cpp
  pipeline/KFold.cpp
  pipeline/training/DBNTrainer.cpp
  pipeline/training/DBNLoader.cpp
  pipeline/DBNSaver.cpp
  pipeline/estimation/Estimator.cpp
  pipeline/estimation/TrainingFrequencyEstimator.cpp
  pipeline/estimation/SumProductEstimator.cpp
  pipeline/estimation/EstimationProcess.cpp
  pipeline/estimation/OfflineEstimation.cpp
  pipeline/estimation/OnlineEstimation.cpp
  pipeline/evaluation/Measure.cpp
  pipeline/evaluation/Estimates.cpp
  pipeline/evaluation/Accuracy.cpp
  pipeline/evaluation/F1Score.cpp
  pipeline/evaluation/EvaluationAggregator.cpp
  pipeline/Pipeline.cpp
  utils/EigenExtensions.cpp
  utils/Mosquitto.cpp
  pgm/inference/MessageNode.cpp
  pgm/inference/MessageNode.h
  pgm/inference/FactorNode.cpp
  pgm/inference/FactorNode.h
  pgm/inference/VariableNode.cpp
  pgm/inference/VariableNode.h
  pgm/inference/FactorGraph.cpp
  pgm/inference/FactorGraph.h)

target_include_directories(tomcat_model_lib PRIVATE
    "${CMAKE_CURRENT_SOURCE_DIR}/../" ${Boost_INCLUDE_DIR} ${GSL_INCLUDE_DIRS})

# Todo make code coverage work on Linux as well.
if(APPLE
   AND CODE_COVERAGE
   AND CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
  # Add required flags (GCC & LLVM/Clang)
  message("Building with code coverage")
  target_compile_options(
    tomcat -O0 # no optimization
    -g # generate debug info
    --coverage # sets all required flags
  )
  if(CMAKE_VERSION VERSION_GREATER_EQUAL 3.13)
    target_link_options(tomcat PUBLIC --coverage)
  else()
    target_link_libraries(tomcat PUBLIC --coverage)
  endif()
endif()

add_executable(TomcatModel main.cpp Tomcat.cpp Tomcat.h)
target_include_directories(TomcatModel PRIVATE ${Boost_INCLUDE_DIR}
                                               ${Mosquitto_INCLUDE_DIRS}
                                               "${CMAKE_CURRENT_SOURCE_DIR}/../"
                                               )
target_link_libraries(
  TomcatModel
  PRIVATE tomcat_model_lib
          ${Boost_LIBRARIES}
          Eigen3::Eigen
          GSL::gsl
          GSL::gslcblas
          fmt::fmt
          nlohmann_json::nlohmann_json
          ${Mosquitto_LIBRARIES})

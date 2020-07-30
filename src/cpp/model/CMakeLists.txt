cmake_minimum_required(VERSION 3.10)

project(model)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# External dependencies

# Eigen is a C++ library used for matrix and vector operations.
find_package(Eigen3 REQUIRED NO_MODULE)

# GSL is a C library used for sampling from a determined distribution.
find_package(GSL REQUIRED)

# Boost is used for several requirements in the project.
find_package(Boost 1.69
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

include_directories(${Boost_INCLUDE_DIR})

# Header files inside model folder
file(GLOB tomcat_model_headers CONFIGURE_DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/*.h)

# Header and cpp files
add_library(tomcat_model_libs
        STATIC
        ${tomcat_model_headers}
        NodeMetadata.cpp
        ConstantNode.cpp
        RandomVariableNode.cpp
        CategoricalCPD.cpp
        ContinuousCPD.cpp
        GaussianCPD.cpp
        DirichletCPD.cpp
        DynamicBayesNet.cpp
        Sampler.cpp
        AncestralSampler.cpp)

add_executable(TomcatModel main.cpp)
target_link_libraries(TomcatModel PUBLIC tomcat_model_libs ${Boost_LIBRARIES} Eigen3::Eigen GSL::gsl GSL::gslcblas fmt::fmt)
cmake_minimum_required(VERSION 3.10)

project(model)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# External dependencies
find_package(Eigen3 REQUIRED NO_MODULE)
find_package(GSL REQUIRED)
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


include_directories(${Boost_INCLUDE_DIR} ${CMAKE_CURRENT_SOURCE_DIR})

# Header files inside model folder
file(GLOB tomcat_model_headers CONFIGURE_DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/*.h)

# Header and cpp files
add_library(tomcat_model_libs
        STATIC
        ${tomcat_model_headers}
        NodeMetadata.cpp
        ConstantNumericNode.cpp
        ConstantVectorNode.cpp
        RandomVariableNumericNode.cpp
        CategoricalCPD.cpp
        GaussianCPD.cpp
        DirichletCPD.cpp)

add_executable(TomcatModel main.cpp)
target_link_libraries(TomcatModel PUBLIC tomcat_model_libs ${Boost_LIBRARIES} Eigen3::Eigen GSL::gsl GSL::gslcblas)
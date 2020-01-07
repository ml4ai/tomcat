FROM        ubuntu:18.04
MAINTAINER  Paul D. Hein <pauldhein@email.arizona.edu>
CMD         bash

RUN apt-get update && apt-get install -y --no-install-recommends apt-utils
RUN apt-get update && apt-get install -y software-properties-common
RUN add-apt-repository ppa:ubuntu-toolchain-r/test
RUN apt-get update

# ------------------------------------------------------------------------------
# Setup C++ and Python3
RUN apt-get install -y build-essential g++-9 gcc-9
RUN update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-9 60 --slave /usr/bin/g++ g++ /usr/bin/g++-9
RUN apt-get -y install python3-pip python3-dev python3-numpy
RUN echo  'alias python=python3' >> ~/.bashrc
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Add common required packages via apt-get and pip
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get upgrade -y
RUN apt-get install -y lcov curl git sudo doxygen ffmpeg make wget pkg-config openjdk-8-jdk
RUN apt-get install -y libfmt-dev libopenblas-dev portaudio19-dev libsndfile1-dev
RUN pip3 install exhale recommonmark sphinx-rtd-theme
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Adding the latest CMake
RUN wget https://cmake.org/files/v3.16/cmake-3.16.2-Linux-x86_64.sh
RUN mkdir /opt/cmake
RUN sh /cmake-3.16.2-Linux-x86_64.sh --prefix=/opt/cmake --skip-license
RUN ln -s /opt/cmake/bin/cmake /usr/local/bin/cmake
RUN cmake --version
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Adding latest version of boost
RUN cd /home && wget https://dl.bintray.com/boostorg/release/1.71.0/source/boost_1_71_0.tar.gz \
  && tar xfz boost_1_71_0.tar.gz \
  && rm boost_1_71_0.tar.gz \
  && cd boost_1_71_0 \
  && ./bootstrap.sh --prefix=/usr/local \
  && ./b2 install \
  && cd /home \
  && rm -rf boost_1_71_0
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Adding latest version of DLIB
# WORKDIR /
# RUN wget http://dlib.net/files/dlib-19.19.tar.bz2
# RUN tar xvf dlib-19.19.tar.bz2
#
# RUN mkdir -p /dlib-19.19/build
# WORKDIR /dlib-19.19/build
# RUN cmake ..
# RUN cmake --build . --config Release
# RUN sudo make install
# RUN sudo ldconfig
# WORKDIR /
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Installing OpenCV from source
RUN apt-get install -y libgtk-3-dev libavcodec-dev libavformat-dev \
  libswscale-dev libv4l-dev libxvidcore-dev libx264-dev libjpeg-dev \
  libpng-dev libtiff-dev gfortran openexr libatlas-base-dev libtbb2 \
  libtbb-dev libdc1394-22-dev


RUN mkdir -p /opencv_build
WORKDIR /opencv_build
RUN git clone https://github.com/opencv/opencv.git
RUN git clone https://github.com/opencv/opencv_contrib.git

RUN mkdir -p /opencv_build/opencv/build
WORKDIR /opencv_build/opencv/build
RUN cmake -D CMAKE_BUILD_TYPE=RELEASE \
    -D CMAKE_INSTALL_PREFIX=/usr/local \
    -D INSTALL_C_EXAMPLES=ON \
    -D INSTALL_PYTHON_EXAMPLES=ON \
    -D OPENCV_GENERATE_PKGCONFIG=ON \
    -D OPENCV_EXTRA_MODULES_PATH=/opencv_build/opencv_contrib/modules \
    -D BUILD_EXAMPLES=ON ..

RUN make -j4
RUN sudo make install
RUN pkg-config --modversion opencv4
# ------------------------------------------------------------------------------

RUN apt-get install -y zip unzip


# ------------------------------------------------------------------------------
# Setup the tomcat repository
WORKDIR /
RUN git clone https://github.com/ml4ai/tomcat.git
ENV TOMCAT=/tomcat

WORKDIR /tomcat/data/
# Download and extract the manually-created worlds from vanga.
RUN curl -O http://vanga.sista.arizona.edu/tomcat/data/worlds.tgz
RUN tar -xvzf worlds.tgz && rm -rf worlds.tgz

# Download and extract the OpenFace models
RUN curl -O http://vision.cs.arizona.edu/adarsh/OpenFace_models.tgz
RUN tar -xvzf OpenFace_models.tgz && rm -rf OpenFace_models.tgz

WORKDIR /tomcat/external
RUN curl -L https://github.com/davisking/dlib/archive/471c3d30e181a40942177a4358aa0496273d2108.zip -o dlib.zip
RUN unzip dlib.zip
RUN mv dlib-471c3d30e181a40942177a4358aa0496273d2108 dlib
ENV dlib_DIR=/tomcat/external/dlib
ENV TRAVIS=1

RUN apt-get install -y zlib1g-dev

# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Installing ToMCAT
RUN mkdir -p /tomcat/build

WORKDIR /tomcat/build/
RUN cmake -std=c++17 /tomcat
RUN make -j4
RUN make -j Minecraft

WORKDIR /tomcat
# ------------------------------------------------------------------------------

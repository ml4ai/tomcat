FROM        ubuntu:18.04
MAINTAINER  Paul D. Hein <pauldhein@email.arizona.edu>
CMD         bash

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    apt-utils software-properties-common && \
    add-apt-repository ppa:ubuntu-toolchain-r/test && \
    apt-get update && \
    apt-get install -y \
      build-essential \
      g++-9 gcc-9 \
      zlib1g-dev \
      python3-pip python3-dev \
      curl \
      git \
      sudo \
      doxygen \
      ffmpeg \
      make \
      wget \
      pkg-config \
      openjdk-8-jdk \
      libfmt-dev \
      libopenblas-dev \
      portaudio19-dev \
      libsndfile1-dev \
      zip unzip \
      libgtk-3-dev libavcodec-dev libavformat-dev \
      libswscale-dev libv4l-dev libxvidcore-dev libx264-dev libjpeg-dev \
      libpng-dev libtiff-dev gfortran openexr libatlas-base-dev libtbb2 \
      libtbb-dev libdc1394-22-dev

# ------------------------------------------------------------------------------
# Setup C++ and Python3
RUN update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-9 60 --slave /usr/bin/g++ g++ /usr/bin/g++-9
RUN echo  'alias python=python3' >> ~/.bashrc
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Setup the tomcat repository
RUN git clone https://github.com/ml4ai/tomcat.git
WORKDIR /tomcat
ENV TOMCAT=/tomcat
RUN ./tools/install.sh


# ------------------------------------------------------------------------------
# Add common required packages via apt-get and pip
RUN pip3 install exhale recommonmark sphinx-rtd-theme
# ------------------------------------------------------------------------------

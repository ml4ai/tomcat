# Every Dockerfile needs a FROM statement as the first executable line,
# this is the OS you will be using
FROM ubuntu:kinetic
CMD bash

# set this to avoid timezone prompt during apt-get installs
ENV DEBIAN_FRONTEND noninteractive

RUN echo "Installing bash utils:"
# Install anything you need that is not included in your OS.
RUN  apt-get update && apt-get -y install \
    libssl-dev \
    build-essential \
    cmake \
    cmake-gui \
    cmake-curses-gui \
    make \
    git \
    gcc \
    fakeroot \
    devscripts \
    dh-make \
    lsb-release 

RUN echo "Installation done:"
RUN pwd
RUN ls

#RUN echo "setting working directory:"
# set to project dir
COPY . /c_hello_world
WORKDIR /c_hello_world

RUN pwd
RUN ls

# Build the executable
RUN echo "Building:"
RUN rm -rf build  \
&& mkdir build  \
&& cd build \
&& cmake .. \
&& make -j `nproc` c_hello_world \
&& cd .. 

# Run the executable
ENTRYPOINT ./build/c_hello_world



FROM ubuntu:latest


RUN apt-get update && apt-get install -y sudo software-properties-common g++

RUN apt-get update \
    && apt-get -y --no-install-recommends install \
      ca-certificates \
      libboost-all-dev \
      pkg-config \
      cmake \
      make \
      git

COPY . /cpp_template_agent
WORKDIR /cpp_template_agent

# Build the agent
RUN rm -rf build \
    && mkdir build \
    && cd build \
    && cmake .. \
    && make -j `nproc` cpp_template_agent \
    && cd ..

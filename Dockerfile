FROM ubuntu:20.04

# build; docker build -t openface .
# run container: docker run --rm -it -u $(id -u ${USER}):$(id -g ${USER}) -v $HOME:$HOME openface bash
# run sensor on a file as ./build/bin/faceSensor --mloc /home/tomcat/Applications/tomcat/data/OpenFace_models --indent -f openface.mov
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install git-core curl sudo g++ nlohmann-json3-dev wget software-properties-common unzip -y

RUN useradd -m tomcat && echo "tomcat:tomcat" | chpasswd && adduser tomcat sudo
WORKDIR /home/tomcat/Applications
ENV USER tomcat

#RUN git clone https://github.com/ml4ai/tomcat.git
COPY ./ ./tomcat/

RUN cd tomcat && ./tools/install
RUN echo "Done tool install"

RUN cd tomcat/build && cmake .. && make -j faceSensor

CMD /bin/bash
from        ubuntu:19.10
maintainer  Adarsh Pyarelal <adarsh@arizona.edu>

workdir /

run git clone https://github.com/ml4ai/tomcat && cd tomcat && ./tools/install.sh

# docker pull $CACHE_IMAGE:latest
docker run -itd --rm --name tomcat-test tomcat

docker exec tomcat-test make test
docker exec --workdir /tomcat/build tomcat-test make -j test
docker exec --workdir /tomcat/build tomcat-test ./bin/test --mission ../external/malmo/sample_missions/default_world_1.xml
docker exec --workdir /tomcat/build tomcat-test make docs
docker cp tomcat-test:/tomcat/build/docs ~/Desktop

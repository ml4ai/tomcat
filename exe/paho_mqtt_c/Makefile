# Build a docker image from a Dockerfile in the same directory as this Makefile

image="paho_mqtt_c_image"
container="paho_mqtt_c_container"
network="host"


# run the container from scratch
all:
	make build run

# build the image from scratch
.PHONY: build
build:
	docker build . -t $(image)

# build the container from the image and start it
.PHONY: run
run:
	docker run --network=$(network) --name=$(container) $(image)

# start the container
.PHONY: start
start:
	docker start $(container)

# stop the container
.PHONY: stop
stop:
	docker stop $(container)


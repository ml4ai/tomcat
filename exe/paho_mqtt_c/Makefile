# Build a docker image from a Dockerfile in the same directory as this Makefile

image="paho_mqtt_c_image"
container="paho_mqtt_c_container"
network="host"


all:
	make build run

.PHONY: build
build:
	docker build . -t $(image)

.PHONY: run
run:
	docker run --network=$(network) --name=$(container) $(image)

.PHONY: start
start:
	docker start $(container)

.PHONY: stop
stop:
	docker stop $(container)

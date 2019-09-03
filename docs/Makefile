all: docs

doxygen-tags/opencv.tag:
	mkdir -p doxygen-tags;
	curl https://docs.opencv.org/4.1.1/opencv.tag -o $@

docs: doxygen-tags/opencv.tag
	doxygen

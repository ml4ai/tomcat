MALMO_DIR:=external/malmo

XS3P_XSL_URL_STUB:=https://raw.githubusercontent.com/bitfehler/xs3p/1b71310dd1e8b9e4087cf6120856c5f701bd336b
XS3P_XSL:=external/malmo/Schemas/xs3p.xsl

$(XS3P_XSL):
	curl $(XS3P_XSL_URL_STUB)/$(@F) -o $@

install: $(XS3P_XSL)
	pip install conan numpy
	mkdir -p build
	cd build \
	  && conan install .. -b missing \
	  && cmake .. \
	  && cmake --build . -- -j

# Launch the Minecraft client on *nix (basically, non-Windows) systems
launch_unix:
	./external/malmo/Minecraft/launchClient.sh

run_example_mission:
	./build/bin/example

clean:
	rm -rf build

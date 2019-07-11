MALMO_DIR:=external/malmo

install:
	pip install conan
	wget https://raw.githubusercontent.com/bitfehler/xs3p/1b71310dd1e8b9e4087cf6120856c5f701bd336b/xs3p.xsl \
	  -P external/malmo/Schemas
	mkdir -p build
	cd build; conan install ..; cmake ..; cmake --build . -- -j

launch_unix:
	./external/malmo/Minecraft/launchClient.sh

run_python_example:
	cd external/malmo/Malmo/samples/Python_examples; python run_mission.py

clean:
	rm -rf build

MALMO_DIR:=external/malmo
MALMOPYTHON_MODULE_DIR:=build/$(MALMO_DIR)/

install:
	cd external; git clone https://github.com/microsoft/malmo
	wget https://raw.githubusercontent.com/bitfehler/xs3p/1b71310dd1e8b9e4087cf6120856c5f701bd336b/xs3p.xsl \
	  -P external/malmo/Schemas
	mkdir -p build
	cd build; conan install ..; cmake ..; make -j

launch_unix:
	./external/malmo/Minecraft/launchClient.sh

run_python_example:
	cd external/malmo/Malmo/samples/Python_examples; python run_mission.py

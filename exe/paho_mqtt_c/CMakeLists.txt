#
# Paho MQTT C project
#

cmake_minimum_required(VERSION 3.16)
project(paho_mqtt_c_test LANGUAGES C)

set(PAHO_MQTT_C_LIBRARIES ~/paho_mqtt_c/paho.mqtt.c/build/output )
set(PAHO_MQTT_C_INCLUDE_DIRS ~/paho_mqtt_c/paho.mqtt.c/include )


find_package(
    OpenSSL
    REQUIRED
)

add_executable(
    paho_mqtt_c_test main.c
)

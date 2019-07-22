#include <pybind11/numpy.h>
#include <pybind11/pybind11.h>
#include <pybind11/stl.h>
#include "LocalAgent.h"

namespace py = pybind11;

PYBIND11_MODULE(tomcatPython, m) {
  py::class_<LocalAgent>(m, "LocalAgent")
    .def(py::init<>())
    .def("sendCommand", &LocalAgent::sendCommand)
    .def("set_mission", &LocalAgent::set_mission)
    .def("startMission", &LocalAgent::startMission)
    ;
}

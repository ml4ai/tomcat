#include "LocalAgent.h"
#include <pybind11/numpy.h>
#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

namespace py = pybind11;
using namespace tomcat;

PYBIND11_MODULE(pytomcat, m) {
  py::class_<LocalAgent>(m, "LocalAgent")
      .def(py::init<>())
      .def("sendCommand", &LocalAgent::sendCommand)
      .def("set_mission", &LocalAgent::setMission)
      .def("startMission", &LocalAgent::startMission);
}

#include "sample.h"

#include <pybind11/pybind11.h>
#include <pybind11/stl.h>
namespace py = pybind11;

#include <algorithm>


std::vector<int> reverse(std::vector<int> xs) {
    auto c_plus_plus = 11;

    std::reverse(xs.begin(), xs.end());
    return xs;
}


PYBIND11_PLUGIN(sample) {
    py::module m("sample", "pybind11 example plugin");

    m.attr("N") = N;
    m.def("square_float", &square<float>);
    m.def("reverse", &reverse);

    py::class_<Hz>(m, "Hz")
        .def(py::init<>())
        .def(py::init<const Hz&>())
        .def_readwrite("a", &Hz::a)
        .def_readwrite("b", &Hz::b)
        .def("__str__", &Hz::__str__)
    ;

    py::class_<Fail>(m, "Fail")
        .def_static("fail_assert", &Fail::fail_assert)
        .def_static("index_out_of_bounds", &Fail::index_out_of_bounds)
        .def_static("page_fault", &Fail::page_fault)
        .def_static("infinite_recursion", &Fail::infinite_recursion)
    ;

    return m.ptr();
}

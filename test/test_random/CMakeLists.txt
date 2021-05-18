cmake_minimum_required(VERSION 3.0)

enable_language(Fortran)

project(test_random_module Fortran)

set(EXECUTABLE test_random.out)

add_executable(${EXECUTABLE}
    test_random.f90
    random.f90
)
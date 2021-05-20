cmake_minimum_required(VERSION 3.0)
enable_testing()
enable_language(C Fortran)
project(test_module Fortran)

set(Test_Names)

list(APPEND Test_Names "test_random")

create_test_sourcelist(Test_Src_List main.c ${Test_Names})

set(Test_Files)

list(APPEND Test_Files "test_random.f90")

set(Target testsuite)
add_library(${Target}_fortran ${Test_Files} random.f90)

add_executable(${Target} main.c)
target_link_libraries(${Target} ${Target}_fortran)

add_test(NAME "test_random"
    COMMAND $<TARGET_FILE:${Target}> "test_random")

cmake_minimum_required(VERSION 3.0)
enable_language(Fortran)
project(test_module Fortran)

set(RANDOM_MOD random_mod)
set(RANDOM_TEST test_random)
add_subdirectory(src)
add_subdirectory(test)

# target_link_libraries(${RANDOM_TEST}
#     PRIVATE
#     ${RANDOM_MOD}
# )
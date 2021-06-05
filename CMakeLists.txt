### プロジェクト全体設定
cmake_minimum_required(VERSION 3.0)

project(ctest_Fortran_lib Fortran)
# project名の設定

enable_language(Fortran)

### モジュールファイルの出力ディレクトリパスの設定
set(LIB_MOD_DIR ${CMAKE_CURRENT_BINARY_DIR}/mod_files/)

enable_testing()

add_subdirectory(src)
add_subdirectory(test)

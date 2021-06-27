### テスト追加のマクロ定義

## ルール
# モジュールファイル名:: ${name}.f90
# テスト対象モジュール名:: ${name}_mod
# テストファイル名:: test_${name}.f90
# 実行ファイル名:: test_${name}.exe

macro(ADDTEST name)

    ### テスト用プログラムのビルド設定
    add_executable(test_${name} test_${name}.f90)

    ### 参照するmodファイルのディレクトリとリンクするライブラリの設定
    target_link_libraries(test_${name} ${name}_mod ${ARGN})
    set_target_properties(test_${name}
        PROPERTIES
        Fortran_MODULE_DIRECTORY ${LIB_MOD_DIR}
    )

    ### テストの登録
    add_test(NAME ${name}
        COMMAND $<TARGET_FILE:test_${name}> ${CMAKE_CURRENT_BINARY_DIR}
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        )

endmacro(ADDTEST)

### テストディレクトリの追加
add_subdirectory(test_random)
add_subdirectory(test_array)
add_subdirectory(test_splay_node)
add_subdirectory(test_merge_sort)

cmake_minimum_required(VERSION 3.13)
project(YuLang VERSION "0.0.7")

# # set compiler path
# set(CMAKE_C_COMPILER "/usr/local/opt/gcc/bin/gcc-13")
# set(CMAKE_CXX_COMPILER "/usr/local/opt/gcc/bin/g++-13")
# set(CMAKE_CXX_COMPILER_ID "GNU")

# C++17 standard support
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# enable all warnings and treat them as errors
if(MSVC)
  add_compile_options(/W3 /WX)
elseif(CMAKE_CXX_COMPILER_ID MATCHES "GNU")
  add_compile_options(-Wall -Wno-init-list-lifetime -Werror)
else()
  add_compile_options(-Wall -Werror)
endif()

# some definitions
add_compile_definitions(APP_NAME="YuLang Compiler")
add_compile_definitions(APP_VERSION="${PROJECT_VERSION}")
add_compile_definitions(APP_VERSION_MAJOR=${PROJECT_VERSION_MAJOR})
add_compile_definitions(APP_VERSION_MINOR=${PROJECT_VERSION_MINOR})
add_compile_definitions(APP_VERSION_PATCH=${PROJECT_VERSION_PATCH})

# find LLVM
find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

# project include directories
include_directories(src)
include_directories(3rdparty/xstl)
include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

# all of C++ source files
file(GLOB_RECURSE SOURCES "src/*.cpp")

# executable
add_executable(yuc ${SOURCES})
link_directories(${LLVM_LIBRARY_DIRS})
target_link_libraries(yuc LLVM)

# Build Yu sources on every build so edits to libraries/examples are noticed.
add_custom_target(yu_examples ALL
  COMMAND make -C ${CMAKE_SOURCE_DIR} BUILD_DIR=${CMAKE_BINARY_DIR}
          DEBUG=$<IF:$<CONFIG:Debug>,1,0>
  DEPENDS yuc
  VERBATIM
)

include(CTest)
if(BUILD_TESTING)
  find_package(Python3 REQUIRED COMPONENTS Interpreter)
  add_test(NAME llvm_backend
    COMMAND ${Python3_EXECUTABLE} ${CMAKE_SOURCE_DIR}/utils/test_backend.py
            --yuc $<TARGET_FILE:yuc> --llvm-bin ${LLVM_TOOLS_BINARY_DIR}
            --cc ${CMAKE_C_COMPILER})
  add_test(NAME examples
    COMMAND bash ${CMAKE_SOURCE_DIR}/utils/run_test.sh ${CMAKE_BINARY_DIR})
  set_tests_properties(llvm_backend examples PROPERTIES TIMEOUT 120)
endif()

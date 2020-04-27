cmake_minimum_required(VERSION 3.5)
project(YuLang VERSION "0.0.3")

# # set compiler path
# set(CMAKE_C_COMPILER "/usr/local/opt/gcc/bin/gcc-9")
# set(CMAKE_CXX_COMPILER "/usr/local/opt/gcc/bin/g++-9")

# C++17 standard support
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# enable all warnings and treat them as errors
if(MSVC)
  add_compile_options(/W3 /WX)
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
llvm_map_components_to_libnames(LLVM_LIBS all)
target_link_libraries(yuc ${LLVM_LIBS})

# build Yu standard library and examples after generation
add_custom_command(
  TARGET yuc
  POST_BUILD
  COMMAND make -C ${CMAKE_SOURCE_DIR} BUILD_DIR=${CMAKE_BINARY_DIR}
)

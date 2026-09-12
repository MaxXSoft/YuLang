cmake_minimum_required(VERSION 3.28)
project(YuLang VERSION "0.0.7")

# find LLVM
# LLVM's package version file only accepts matching major.minor versions, so
# check our minimum separately to allow newer LLVM releases too.
find_package(LLVM REQUIRED CONFIG)
if(LLVM_VERSION_MAJOR LESS 21)
  message(FATAL_ERROR "YuLang requires LLVM 21 or newer; found ${LLVM_PACKAGE_VERSION}")
endif()
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

# all of C++ source files
file(GLOB_RECURSE SOURCES CONFIGURE_DEPENDS "src/*.cpp")

# executable
add_executable(yuc ${SOURCES})
target_compile_features(yuc PRIVATE cxx_std_17)
set_target_properties(yuc PROPERTIES
  CXX_EXTENSIONS OFF
  COMPILE_WARNING_AS_ERROR ON
)

# Enable warnings; CMake supplies the compiler-specific warning-as-error flag.
if(MSVC)
  target_compile_options(yuc PRIVATE /W3)
elseif(CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
  target_compile_options(yuc PRIVATE -Wall -Wno-init-list-lifetime)
else()
  target_compile_options(yuc PRIVATE -Wall)
endif()

separate_arguments(LLVM_DEFINITION_FLAGS NATIVE_COMMAND "${LLVM_DEFINITIONS}")
target_compile_definitions(yuc PRIVATE
  APP_NAME="YuLang Compiler"
  APP_VERSION="${PROJECT_VERSION}"
  APP_VERSION_MAJOR=${PROJECT_VERSION_MAJOR}
  APP_VERSION_MINOR=${PROJECT_VERSION_MINOR}
  APP_VERSION_PATCH=${PROJECT_VERSION_PATCH}
  ${LLVM_DEFINITION_FLAGS}
)
target_include_directories(yuc PRIVATE src 3rdparty/xstl)
target_include_directories(yuc SYSTEM PRIVATE ${LLVM_INCLUDE_DIRS})
target_link_libraries(yuc PRIVATE LLVM)

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
            --cc ${CMAKE_C_COMPILER} "--sdk=${CMAKE_OSX_SYSROOT}")
  add_test(NAME examples
    COMMAND bash ${CMAKE_SOURCE_DIR}/utils/run_test.sh ${CMAKE_BINARY_DIR})
  set_tests_properties(llvm_backend examples PROPERTIES TIMEOUT 120)
endif()

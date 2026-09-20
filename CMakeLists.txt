cmake_minimum_required(VERSION 3.28)
project(YuLang VERSION "0.0.8")

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

# Compile Yu sources with the compiler built by this project.
file(GLOB_RECURSE YU_LIBRARY_SOURCES CONFIGURE_DEPENDS "lib/*.yu")

function(compile_yu source output)
  file(RELATIVE_PATH relative "${CMAKE_CURRENT_SOURCE_DIR}" "${source}")
  set(object "${CMAKE_CURRENT_BINARY_DIR}/obj/$<CONFIG>/${relative}.o")
  get_filename_component(directory "${object}" DIRECTORY)
  add_custom_command(OUTPUT "${object}"
    COMMAND ${CMAKE_COMMAND} -E make_directory "${directory}"
    COMMAND yuc -Werror -O $<IF:$<CONFIG:Debug>,0,2>
            -I "${CMAKE_CURRENT_SOURCE_DIR}/lib" -ot obj "${source}" -o "${object}" -MD
    DEPFILE "${object}.d"
    DEPENDS yuc "${source}" "${CMAKE_CURRENT_LIST_FILE}"
    VERBATIM)
  set_source_files_properties("${object}" PROPERTIES EXTERNAL_OBJECT TRUE)
  set(${output} "${object}" PARENT_SCOPE)
endfunction()

foreach(source IN LISTS YU_LIBRARY_SOURCES)
  compile_yu("${source}" object)
  list(APPEND YU_LIBRARY_OBJECTS "${object}")
endforeach()
add_library(yu STATIC ${YU_LIBRARY_OBJECTS})
set_target_properties(yu PROPERTIES LINKER_LANGUAGE C)

file(GLOB EXAMPLE_SOURCES CONFIGURE_DEPENDS "examples/*.yu")
add_custom_target(yu_examples)
foreach(source IN LISTS EXAMPLE_SOURCES)
  get_filename_component(example "${source}" NAME_WE)
  compile_yu("${source}" object)
  add_executable(example_${example} "${object}")
  set_target_properties(example_${example} PROPERTIES
    LINKER_LANGUAGE C
    OUTPUT_NAME "${example}"
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/examples")
  target_link_libraries(example_${example} PRIVATE yu)
  # Match yuc's default static relocation model on Linux.
  if(CMAKE_SYSTEM_NAME STREQUAL "Linux")
    target_link_options(example_${example} PRIVATE -no-pie)
  endif()
  add_dependencies(yu_examples example_${example})
endforeach()

include(CTest)
if(BUILD_TESTING)
  find_package(Python3 REQUIRED COMPONENTS Interpreter)
  function(add_runtime_regression name)
    set(module_args)
    foreach(module IN LISTS ARGN)
      list(APPEND module_args --module "${CMAKE_SOURCE_DIR}/lib/${module}.yu")
    endforeach()
    add_test(NAME runtime.${name}
      COMMAND ${Python3_EXECUTABLE} ${CMAKE_SOURCE_DIR}/tests/runtime/run.py
              --yuc $<TARGET_FILE:yuc> --cc ${CMAKE_C_COMPILER}
              "--sdk=${CMAKE_OSX_SYSROOT}"
              --source ${CMAKE_SOURCE_DIR}/tests/runtime/${name}.yu
              --helper ${CMAKE_SOURCE_DIR}/tests/runtime/${name}.c
              --work-dir ${CMAKE_BINARY_DIR}/test ${module_args})
    set_tests_properties(runtime.${name} PROPERTIES TIMEOUT 120 LABELS runtime)
  endfunction()
  add_runtime_regression(bool_casts)
  add_runtime_regression(constant_arithmetic)
  add_runtime_regression(constant_scopes)
  add_runtime_regression(struct_layout)
  add_runtime_regression(global_initializers)
  add_test(NAME runtime.layout_targets
    COMMAND ${Python3_EXECUTABLE} ${CMAKE_SOURCE_DIR}/tests/runtime/test_layout_targets.py
            --yuc $<TARGET_FILE:yuc> --llvm-bin ${LLVM_TOOLS_BINARY_DIR}
            --work-dir ${CMAKE_BINARY_DIR}/test)
  set_tests_properties(runtime.layout_targets PROPERTIES TIMEOUT 120 LABELS runtime)
  add_executable(test_usedef tests/mid/test_usedef.cpp src/mid/usedef.cpp)
  target_compile_features(test_usedef PRIVATE cxx_std_17)
  target_include_directories(test_usedef PRIVATE src 3rdparty/xstl)
  set_target_properties(test_usedef PROPERTIES CXX_EXTENSIONS OFF)
  add_test(NAME mid.usedef COMMAND test_usedef)
  add_test(NAME front.constants
    COMMAND ${Python3_EXECUTABLE} ${CMAKE_SOURCE_DIR}/tests/front/test_constants.py
            --yuc $<TARGET_FILE:yuc> --work-dir ${CMAKE_BINARY_DIR}/test)
  add_test(NAME front.dependencies
    COMMAND ${Python3_EXECUTABLE} ${CMAKE_SOURCE_DIR}/tests/front/test_dependencies.py
            --yuc $<TARGET_FILE:yuc> --work-dir ${CMAKE_BINARY_DIR}/test)
  add_test(NAME front.macros
    COMMAND ${Python3_EXECUTABLE} ${CMAKE_SOURCE_DIR}/tests/front/test_macros.py
            --yuc $<TARGET_FILE:yuc> --work-dir ${CMAKE_BINARY_DIR}/test)
  foreach(level RANGE 0 3)
    add_test(NAME llvm_backend.O${level}
      COMMAND ${Python3_EXECUTABLE} ${CMAKE_SOURCE_DIR}/tests/backend/test_backend.py
              --yuc $<TARGET_FILE:yuc> --llvm-bin ${LLVM_TOOLS_BINARY_DIR}
              --cc ${CMAKE_C_COMPILER} "--sdk=${CMAKE_OSX_SYSROOT}"
              --opt-level ${level} --work-dir ${CMAKE_BINARY_DIR}/test)
    set_tests_properties(llvm_backend.O${level} PROPERTIES
      TIMEOUT 120 LABELS backend)
  endforeach()

  file(GLOB EXAMPLE_OUTPUTS CONFIGURE_DEPENDS
    "tests/example/output/*.out" "tests/example/output/*.regex")
  foreach(expected IN LISTS EXAMPLE_OUTPUTS)
    get_filename_component(example "${expected}" NAME_WE)
    if(expected MATCHES "\\.regex$")
      set(check --pattern)
    else()
      set(check --expected)
    endif()
    set(input "${CMAKE_SOURCE_DIR}/tests/example/input/${example}.in")
    set(input_args)
    if(EXISTS "${input}")
      list(APPEND input_args --input "${input}")
    endif()
    add_test(NAME example.${example}
      COMMAND ${Python3_EXECUTABLE} ${CMAKE_SOURCE_DIR}/tests/example/test_example.py
              --executable $<TARGET_FILE:example_${example}>
              ${check} "${expected}" ${input_args})
    set_tests_properties(example.${example} PROPERTIES
      TIMEOUT 120 LABELS examples)
  endforeach()
endif()

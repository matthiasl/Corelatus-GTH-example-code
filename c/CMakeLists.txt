# Builds the Corelatus GTH example code.
#
# Linux/unix:      make            (or: cmake --preset linux && cmake --build --preset linux)
# Visual Studio:   File -> Open -> Folder, then pick a preset
# Cross to win64:  make win64      (needs the Debian mingw-w64 packages)
#
# See CMakePresets.json for the list of presets.

cmake_minimum_required(VERSION 3.21)
project(gth_c_examples LANGUAGES C)

option(GTH_PEDANTIC "Enable extra compiler warnings (GCC/Clang)" OFF)

# Link the C runtime statically with MSVC so the .exe files run on machines
# without the Visual C++ redistributable installed.
set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")

set(EXAMPLES
  connect_timeslots
  disable
  duplex_lapd
  enable
  install_release
  install_start_script
  job_query
  map
  monitor_dtmf
  playback_file
  print_broadcast_events
  query_set
  raw_xml
  record
  replay_ss7
  save_to_pcap
  unmap
)

#----------------------------------------------------------------------
# version.c: regenerated on every build from 'git describe', but only
# rewritten when its contents change, so it doesn't force relinks.
set(VERSION_C ${CMAKE_CURRENT_BINARY_DIR}/version.c)
add_custom_target(gth_version
  COMMAND ${CMAKE_COMMAND}
          -DSOURCE_DIR=${CMAKE_CURRENT_SOURCE_DIR}
          -DOUTPUT=${VERSION_C}
          -P ${CMAKE_CURRENT_SOURCE_DIR}/cmake/gen_version.cmake
  BYPRODUCTS ${VERSION_C}
  COMMENT "Generating version.c"
  VERBATIM)

#----------------------------------------------------------------------
# Code shared by all the examples
add_library(gth STATIC
  gth_apilib.c
  gth_client_xml_parse.c
  ${VERSION_C})
add_dependencies(gth gth_version)
target_include_directories(gth PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

if(WIN32)
  target_link_libraries(gth PUBLIC ws2_32)
elseif(CMAKE_SYSTEM_NAME STREQUAL "Linux")
  # timer_create() is in librt on glibc older than 2.34
  target_link_libraries(gth PUBLIC rt)
endif()

if(MSVC)
  target_compile_options(gth PUBLIC /W3)
  # The code deliberately uses fopen(), sscanf() etc. for portability.
  target_compile_definitions(gth PUBLIC _CRT_SECURE_NO_WARNINGS)
else()
  target_compile_options(gth PUBLIC -Wall)
  if(GTH_PEDANTIC)
    target_compile_options(gth PUBLIC -Wextra -Wshadow -Wstrict-prototypes)
  endif()
endif()

#----------------------------------------------------------------------
foreach(example IN LISTS EXAMPLES)
  add_executable(${example} ${example}.c)
  target_link_libraries(${example} PRIVATE gth)
endforeach()

# Writes version.c with the git revision and build host.
# Called from CMakeLists.txt as: cmake -DSOURCE_DIR=... -DOUTPUT=... -P gen_version.cmake

set(git_head "unknown")
find_package(Git QUIET)
if(GIT_FOUND)
  execute_process(
    COMMAND ${GIT_EXECUTABLE} describe --abbrev=8 --dirty --always
    WORKING_DIRECTORY ${SOURCE_DIR}
    OUTPUT_VARIABLE describe
    OUTPUT_STRIP_TRAILING_WHITESPACE
    RESULT_VARIABLE result
    ERROR_QUIET)
  if(result EQUAL 0 AND describe)
    set(git_head "${describe}")
  endif()
endif()

cmake_host_system_information(RESULT build_hostname QUERY HOSTNAME)

file(WRITE ${OUTPUT}.tmp
"#include \"gth_apilib.h\"
const char git_head[] = \"${git_head}\";
const char build_hostname[] = \"${build_hostname}\";
")
file(COPY_FILE ${OUTPUT}.tmp ${OUTPUT} ONLY_IF_DIFFERENT)
file(REMOVE ${OUTPUT}.tmp)

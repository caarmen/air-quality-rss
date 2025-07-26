# Generated mostly by ChatGPT.

cmake_minimum_required(VERSION 3.10)
project(gnucobol_microhttpd C)

set(COB_COPY_DIR "${CMAKE_SOURCE_DIR}/src")
# Set C standard and output dir
set(CMAKE_C_STANDARD 11)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
include(cmake/pollutant.cmake)

# C source
set(C_SOURCES
    ${CMAKE_SOURCE_DIR}/src/support/cob/json/cJSON-wrapper.c
)

# COBOL files
set(COB_MAIN ${CMAKE_SOURCE_DIR}/src/air-quality/air-quality-rss.cob)  # Needs -x
set(COB_OTHER
    ${CMAKE_SOURCE_DIR}/src/air-quality/api/air-quality-router.cob
    ${CMAKE_SOURCE_DIR}/src/air-quality/api/air-quality-server.cob
    ${CMAKE_SOURCE_DIR}/src/pollen/service/pollen-data-source.cob
    ${CMAKE_SOURCE_DIR}/src/pollen/service/pollen-parser.cob
    ${CMAKE_SOURCE_DIR}/src/pollen/service/pollen-render.cob
    ${CMAKE_SOURCE_DIR}/src/pollen/service/pollen-service.cob
    ${CMAKE_SOURCE_DIR}/src/support/cob/c-interop/c-string.cob
    ${CMAKE_SOURCE_DIR}/src/support/cob/geo/coordinates.cob
    ${CMAKE_SOURCE_DIR}/src/support/cob/http/curl-write-callback.cob
    ${CMAKE_SOURCE_DIR}/src/support/cob/http/http-client.cob
    ${CMAKE_SOURCE_DIR}/src/support/cob/http/query-param-parser.cob
    ${CMAKE_SOURCE_DIR}/src/support/cob/json/json-parser.cob
    ${CMAKE_SOURCE_DIR}/src/support/cob/rss/render-rss.cob
    ${CMAKE_SOURCE_DIR}/src/support/cob/xml/encode.cob
) # No -x

# Output COBOL objects
set(COB_OBJECTS "")

# Compile cobol files.
function(compile_cobol OUTPUT_VAR COB_FILE)
    # Check if COBOL_FLAGS is provided, otherwise set it to an empty string
    set(COBOL_FLAGS "")
    if(ARGC GREATER 2)
        set(COBOL_FLAGS ${ARGV2})
    endif()
    get_filename_component(FNAME_WE ${COB_FILE} NAME_WE)
    string(REPLACE "/" "_" OBJ_NAME ${FNAME_WE})
    set(OBJ_PATH "${CMAKE_BINARY_DIR}/${OBJ_NAME}.o")
    add_custom_command(
        OUTPUT ${OBJ_PATH}
        COMMAND COB_COPY_DIR=${COB_COPY_DIR} cobc -c ${COBOL_FLAGS} ${COB_FILE} -o ${OBJ_PATH}
        DEPENDS ${COB_FILE}
        COMMENT "Compiling ${COB_FILE} ${COBOL_FLAGS}"
    )

    set(${OUTPUT_VAR} ${OBJ_PATH} PARENT_SCOPE)
endfunction()

# Compile the entry point. It needs -x
compile_cobol(COB_MAIN_OBJ ${COB_MAIN} "-x")
list(APPEND COB_OBJECTS ${COB_MAIN_OBJ})

# Compile other COBOL files.
foreach(COB_FILE IN LISTS COB_OTHER)
    compile_cobol(OBJ_PATH ${COB_FILE})
    list(APPEND COB_OBJECTS ${OBJ_PATH})
endforeach()

# Compile C code
add_library(c_wrappers OBJECT ${C_SOURCES})

# Link everything into the final executable
add_executable(air-quality-rss
    $<TARGET_OBJECTS:c_wrappers>
    ${COB_OBJECTS}
)

target_link_options(air-quality-rss PRIVATE
    -flto=auto
    -ffat-lto-objects
)

if(CMAKE_SYSTEM_NAME STREQUAL "Linux")
    target_link_options(air-quality-rss PRIVATE
        -Wl,--export-dynamic
        -Wl,-Bsymbolic-functions
        -Wl,-z,relro
        -Wl,--no-as-needed
    )
endif()

target_link_libraries(air-quality-rss
    cob
    m
    microhttpd
    curl
    cjson
    pollutant-core
)

# Ensure COBOL objects are built first
add_custom_target(cobol_objects ALL DEPENDS ${COB_OBJECTS})
add_dependencies(air-quality-rss cobol_objects pollutant-core)

if(DEFINED ENV{BUILD_EXAMPLES})
    add_subdirectory(examples)
endif()
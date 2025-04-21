# Generated mostly by ChatGPT.

cmake_minimum_required(VERSION 3.10)
project(gnucobol_microhttpd C)

set(COB_COPY_DIR "${CMAKE_SOURCE_DIR}/src")
# Set C standard and output dir
set(CMAKE_C_STANDARD 11)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

# C source
set(C_SOURCES
    ${CMAKE_SOURCE_DIR}/src/support/json/cJSON-wrapper.c
)

# COBOL files
set(COB_MAIN ${CMAKE_SOURCE_DIR}/src/pollen/pollen-rss.cob)  # Needs -x
set(COB_OTHER
    ${CMAKE_SOURCE_DIR}/src/pollen/api/pollen-router.cob
    ${CMAKE_SOURCE_DIR}/src/pollen/api/pollen-server.cob
    ${CMAKE_SOURCE_DIR}/src/pollen/service/pollen-data-source.cob
    ${CMAKE_SOURCE_DIR}/src/pollen/service/pollen-parser.cob
    ${CMAKE_SOURCE_DIR}/src/pollen/service/pollen-render.cob
    ${CMAKE_SOURCE_DIR}/src/pollen/service/pollen-service.cob
    ${CMAKE_SOURCE_DIR}/src/support/c-interop/c-string.cob
    ${CMAKE_SOURCE_DIR}/src/support/http/http-client.cob
    ${CMAKE_SOURCE_DIR}/src/support/http/curl-write-callback.cob
    ${CMAKE_SOURCE_DIR}/src/support/json/json-parser.cob
    ${CMAKE_SOURCE_DIR}/src/support/math/coordinates.cob
) # No -x

# Output COBOL objects
set(COB_OBJECTS "")

# Compile pollen-rss.cob with -x
set(COB_MAIN_OBJ ${CMAKE_BINARY_DIR}/pollen-rss.o)
add_custom_command(
    OUTPUT ${COB_MAIN_OBJ}
    COMMAND COB_COPY_DIR=${COB_COPY_DIR} cobc -c -x ${COB_MAIN} -o ${COB_MAIN_OBJ}
    DEPENDS ${COB_MAIN}
    COMMENT "Compiling pollen-rss.cob with -x"
)
list(APPEND COB_OBJECTS ${COB_MAIN_OBJ})

# Compile other COBOL files without -x
foreach(COB_FILE IN LISTS COB_OTHER)
    get_filename_component(FNAME_WE ${COB_FILE} NAME_WE)
    string(REPLACE "/" "_" OBJ_NAME ${FNAME_WE})
    set(OBJ_PATH "${CMAKE_BINARY_DIR}/${OBJ_NAME}.o")
    add_custom_command(
        OUTPUT ${OBJ_PATH}
        COMMAND COB_COPY_DIR=${COB_COPY_DIR} cobc -c ${COB_FILE} -o ${OBJ_PATH}
        DEPENDS ${COB_FILE}
        COMMENT "Compiling ${COB_FILE} without -x"
    )
    list(APPEND COB_OBJECTS ${OBJ_PATH})
endforeach()

# Compile C code
add_library(c_wrappers OBJECT ${C_SOURCES})

# Link everything into the final executable
add_executable(pollen-rss
    $<TARGET_OBJECTS:c_wrappers>
    ${COB_OBJECTS}
)

target_link_options(pollen-rss PRIVATE
    -flto=auto
    -ffat-lto-objects
)

if(CMAKE_SYSTEM_NAME STREQUAL "Linux")
    target_link_options(pollen-rss PRIVATE
        -Wl,--export-dynamic
        -Wl,-Bsymbolic-functions
        -Wl,-z,relro
        -Wl,--no-as-needed
    )
endif()

target_link_libraries(pollen-rss
    cob
    m
    microhttpd
    curl
    cjson
)

# Ensure COBOL objects are built first
add_custom_target(cobol_objects ALL DEPENDS ${COB_OBJECTS})
add_dependencies(pollen-rss cobol_objects)


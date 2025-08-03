enable_language(Fortran)
include(ExternalProject)

# Find the NetCDF Fortran and C libraries installed on the system.
find_package(PkgConfig REQUIRED)
pkg_check_modules(NETCDF_FORTRAN REQUIRED netcdf-fortran)
pkg_check_modules(NETCDF REQUIRED netcdf)

# Build the HTTP client library using fpm.
ExternalProject_Add(http_client_external
    PREFIX ${CMAKE_BINARY_DIR}/http-client
    GIT_REPOSITORY https://github.com/fortran-lang/http-client.git
    GIT_TAG main
    CONFIGURE_COMMAND ""
    BUILD_COMMAND fpm build
    BUILD_IN_SOURCE 1
    INSTALL_COMMAND ""
)
add_custom_target(copy_http_files ALL
    COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_BINARY_DIR}/http-client/lib
    COMMAND ${CMAKE_COMMAND} -E copy
        ${CMAKE_BINARY_DIR}/http-client/src/http_client_external/build/*/http/libhttp.a
        ${CMAKE_BINARY_DIR}/http-client/lib/libhttp.a
    COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_BINARY_DIR}/http-client/mod
    COMMAND ${CMAKE_COMMAND} -E copy
        ${CMAKE_BINARY_DIR}/http-client/src/http_client_external/build/*/*.mod
        ${CMAKE_BINARY_DIR}/http-client/mod/
)

# Build the JSON Fortran library using fpm.
ExternalProject_Add(json_fortran_external
    PREFIX ${CMAKE_BINARY_DIR}/json-fortran
    GIT_REPOSITORY https://github.com/jacobwilliams/json-fortran
    GIT_TAG master
    CONFIGURE_COMMAND ""
    BUILD_COMMAND fpm build
    BUILD_IN_SOURCE 1
    INSTALL_COMMAND ""
)
add_custom_target(copy_json_files ALL
    COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_BINARY_DIR}/json-fortran/lib
    COMMAND ${CMAKE_COMMAND} -E copy
        ${CMAKE_BINARY_DIR}/json-fortran/src/json_fortran_external/build/*/json-fortran/libjson-fortran.a
        ${CMAKE_BINARY_DIR}/json-fortran/lib/libjson-fortran.a
    COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_BINARY_DIR}/json-fortran/mod
    COMMAND ${CMAKE_COMMAND} -E copy
        ${CMAKE_BINARY_DIR}/json-fortran/src/json_fortran_external/build/*/*.mod
        ${CMAKE_BINARY_DIR}/json-fortran/mod/
)

# Fortran sources for the pollutant provider.
set(FORTRAN_LIBRARY_SOURCES
    ${CMAKE_SOURCE_DIR}/src/pollutant/provider/prevair/pollutant-calculator.f90
    ${CMAKE_SOURCE_DIR}/src/pollutant/provider/prevair/pollutant-netcdf-data.f90
    ${CMAKE_SOURCE_DIR}/src/pollutant/provider/prevair/pollutant-netcdf-parser.f90
    ${CMAKE_SOURCE_DIR}/src/pollutant/provider/prevair/pollutant-resource-parser.f90
    ${CMAKE_SOURCE_DIR}/src/pollutant/provider/prevair/pollutant-provider.f90
    ${CMAKE_SOURCE_DIR}/src/support/f90/datetime.f90
    ${CMAKE_SOURCE_DIR}/src/support/f90/file-downloader.f90
    ${CMAKE_SOURCE_DIR}/src/support/f90/geo.f90
    ${CMAKE_SOURCE_DIR}/src/support/f90/c-string.f90
)

# Build the Fortran sources into a static library.
add_library(pollutant-core SHARED ${FORTRAN_LIBRARY_SOURCES})
target_include_directories(pollutant-core PUBLIC
    ${NETCDF_FORTRAN_INCLUDE_DIRS}
    ${CMAKE_BINARY_DIR}/http-client/mod
    ${CMAKE_BINARY_DIR}/json-fortran/mod
)
target_link_libraries(pollutant-core
    ${CMAKE_BINARY_DIR}/http-client/lib/libhttp.a
    ${CMAKE_BINARY_DIR}/json-fortran/lib/libjson-fortran.a
    curl
    netcdf
    netcdff
)
target_link_directories(pollutant-core PUBLIC
    ${NETCDF_FORTRAN_LIBRARY_DIRS}
    ${NETCDF_LIBRARY_DIRS}
)

# Build external library dependencies.
add_dependencies(pollutant-core
  http_client_external copy_http_files
  json_fortran_external copy_json_files
)

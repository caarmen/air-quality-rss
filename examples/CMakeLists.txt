# Add example executable
add_executable(example_pollutant_prevair_c example_pollutant_prevair.c)
add_executable(example_pollutant_prevair_f90 example_pollutant_prevair.f90)
add_executable(example_pollutant_atmo_france_c example_pollutant_atmo_france.c)
add_executable(example_pollutant_atmo_france_f90 example_pollutant_atmo_france.f90)

# Set output directory for example binaries
set_target_properties(example_pollutant_prevair_c PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/examples/bin
)
set_target_properties(example_pollutant_prevair_f90 PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/examples/bin
)
set_target_properties(example_pollutant_atmo_france_c PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/examples/bin
)
set_target_properties(example_pollutant_atmo_france_f90 PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/examples/bin
)

target_link_libraries(example_pollutant_prevair_c
    pollutant-core
)
target_link_libraries(example_pollutant_prevair_f90
    pollutant-core
)
target_link_libraries(example_pollutant_atmo_france_c
    pollutant-core
)
target_link_libraries(example_pollutant_atmo_france_f90
    pollutant-core
)

target_include_directories(example_pollutant_prevair_f90 PRIVATE
    ${NETCDF_FORTRAN_INCLUDE_DIRS}
    ${CMAKE_BINARY_DIR}/http-client/mod
    ${CMAKE_BINARY_DIR}/json-fortran/mod
    ${CMAKE_BINARY_DIR}
)
target_include_directories(example_pollutant_atmo_france_f90 PRIVATE
    ${CMAKE_BINARY_DIR}/http-client/mod
    ${CMAKE_BINARY_DIR}/json-fortran/mod
    ${CMAKE_BINARY_DIR}
)

# Make sure the examples build after the library
add_dependencies(example_pollutant_prevair_c pollutant-core)
add_dependencies(example_pollutant_prevair_f90 pollutant-core)
add_dependencies(example_pollutant_atmo_france_c pollutant-core)
add_dependencies(example_pollutant_atmo_france_f90 pollutant-core)

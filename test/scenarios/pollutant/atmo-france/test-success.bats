#!/usr/bin/env bats
# shellcheck shell=bash

load "../../../support/test-actions.bash"

@test "Test atmo france nominal pollutant data" {
    # GIVEN a remote pollutant server running which returns valid data
    # AND a local pollutant server waiting for a request
    # WHEN a request is made to the local pollutant server
    # THEN the local pollutant server should return a valid RSS feed

    # GIVEN a remote pollutant server running which returns valid data
    # AND a local pollutant server waiting for a request
    launch_remote_server "pollutant/atmo-france/ok"

    # WHEN a request is made to the local pollutant server
    call_local_server "/pollutant-rss/atmo-france/tabular?code_zone=75056"

    # THEN the local pollen server should return a valid RSS feed
    [ "${http_status}" -eq 200 ]
    run compare_response "pollutant/atmo-france/ok"
    [ "$status" -eq 0 ]
}

@test "Test atmo france some missing pollutant data" {
    # GIVEN a remote pollutant server running which returns some missing data
    # AND a local pollutant server waiting for a request
    # WHEN a request is made to the local pollutant server
    # THEN the local pollutant server should return a valid RSS feed
    # AND the RSS feed should contain only the valid data

    # GIVEN a remote pollutant server running which returns some valid data
    # AND a local pollutant server waiting for a request
    launch_remote_server "pollutant/atmo-france/some-missing-data"

    # WHEN a request is made to the local pollutant server
    call_local_server "/pollutant-rss/atmo-france/tabular?code_zone=75056"

    # THEN the local pollutant server should return a valid RSS feed
    # AND the RSS feed should contain only the valid data
    [ "${http_status}" -eq 200 ]
    run compare_response "pollutant/atmo-france/some-missing-data"
    [ "$status" -eq 0 ]
}

@test "Test atmo france all missing pollutant data" {
    # GIVEN a remote pollutant server running which returns all missing data
    # AND a local pollutant server waiting for a request
    # WHEN a request is made to the local pollutant server
    # THEN the local pollutant server should return a valid RSS feed
    # AND the RSS feed should contain no entry

    # GIVEN a remote pollutant server running which returns all missing data
    # AND a local pollutant server waiting for a request
    launch_remote_server "pollutant/atmo-france/all-missing-data"

    # WHEN a request is made to the local pollutant server
    call_local_server "/pollutant-rss/atmo-france/tabular?code_zone=75056"

    # THEN the local pollutant server should return a valid RSS feed
    # AND the RSS feed should contain no entry
    [ "${http_status}" -eq 200 ]
    run compare_response "pollutant/atmo-france/all-missing-data"
    [ "$status" -eq 0 ]
}

@test "Test real atmo server pollutant data" {
    skip "Data source currently down: https://www.data.gouv.fr/datasets/indice-de-la-qualite-de-lair-quotidien-par-commune-indice-atmo/#/discussions/68975f646683faf4c10d257f"
    # GIVEN a remote pollutant server running which returns valid data
    # AND a local pollutant server waiting for a request
    # WHEN a request is made to the local pollutant server
    # THEN the local pollutant server should return a valid RSS feed

    # Stop the local pollutant server. We need to restart it configured
    # to use the real remote server.
    teardown
    launch_local_server

    # WHEN a request is made to the local pollutant server
    call_local_server "/pollutant-rss/atmo-france/tabular?code_zone=75056"

    # THEN the local pollutant server should return a valid RSS feed
    [ "$http_status" -eq 200 ]
    # Spot check a couple of values in the response:
    o3_index_count=$(grep -c "O3: " \
        "${test_log_folder}/actual-local-response-body.txt")
    [ "${o3_index_count}" -eq 1 ]

    no2_index_count=$(grep -c "NO2: " \
        "${test_log_folder}/actual-local-response-body.txt")
    [ "${no2_index_count}" -eq 1 ]
}

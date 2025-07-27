#!/usr/bin/env bats
# shellcheck shell=bash

load "../../support/test-actions.bash"

@test "Test nominal pollutant data" {
    # GIVEN a remote pollutant server running which returns valid data
    # AND a local pollutant server waiting for a request
    # WHEN a request is made to the local pollutant server
    # THEN the local pollutant server should return a valid RSS feed

    # GIVEN a remote pollutant server running which returns valid data
    # AND a local pollutant server waiting for a request
    launch_remote_server "pollutant/ok"

    # WHEN a request is made to the local pollutant server
    call_local_server "/pollutant-rss?latitude=48.8439104&longitude=2.3570831"

    # THEN the local pollen server should return a valid RSS feed
    [ "${http_status}" -eq 200 ]
    run compare_response "pollutant/ok"
    [ "$status" -eq 0 ]
}

@test "Test some invalid pollutant data" {
    # GIVEN a remote pollutant server running which returns some valid data
    # AND a local pollutant server waiting for a request
    # WHEN a request is made to the local pollutant server
    # THEN the local pollutant server should return a valid RSS feed
    # AND the RSS feed should contain only the valid data

    # GIVEN a remote pollutant server running which returns some valid data
    # AND a local pollutant server waiting for a request
    launch_remote_server "pollutant/some-invalid-data"

    # WHEN a request is made to the local pollutant server
    call_local_server "/pollutant-rss?latitude=48.8439104&longitude=2.3570831"

    # THEN the local pollutant server should return a valid RSS feed
    # AND the RSS feed should contain only the valid data
    [ "${http_status}" -eq 200 ]
    run compare_response "pollutant/some-invalid-data"
    [ "$status" -eq 0 ]
}

@test "Test all invalid pollutant data" {
    # GIVEN a remote pollutant server running which returns all invalid data
    # AND a local pollutant server waiting for a request
    # WHEN a request is made to the local pollutant server
    # THEN the local pollutant server should return a valid RSS feed
    # AND the RSS feed should contain no entry

    # GIVEN a remote pollutant server running which returns all invalid data
    # AND a local pollutant server waiting for a request
    launch_remote_server "pollutant/all-invalid-data"

    # WHEN a request is made to the local pollutant server
    call_local_server "/pollutant-rss?latitude=48.8439104&longitude=2.3570831"

    # THEN the local pollutant server should return a valid RSS feed
    # AND the RSS feed should contain no entry
    [ "${http_status}" -eq 200 ]
    run compare_response "pollutant/all-invalid-data"
    [ "$status" -eq 0 ]
}

@test "Test all missing pollutant data" {
    # GIVEN a remote pollutant server running which returns all missing data
    # AND a local pollutant server waiting for a request
    # WHEN a request is made to the local pollutant server
    # THEN the local pollutant server should return a valid RSS feed
    # AND the RSS feed should contain no entry

    # GIVEN a remote pollutant server running which returns all missing data
    # AND a local pollutant server waiting for a request
    launch_remote_server "pollutant/all-missing-data"

    # WHEN a request is made to the local pollutant server
    call_local_server "/pollutant-rss?latitude=48.8439104&longitude=2.3570831"

    # THEN the local pollutant server should return a valid RSS feed
    # AND the RSS feed should contain no entry
    [ "${http_status}" -eq 200 ]
    run compare_response "pollutant/all-missing-data"
    [ "$status" -eq 0 ]
}

@test "Test real server pollutant data" {
    # GIVEN a remote pollutant server running which returns valid data
    # AND a local pollutant server waiting for a request
    # WHEN a request is made to the local pollutant server
    # THEN the local pollutant server should return a valid RSS feed

    # Stop the local pollutant server. We need to restart it configured
    # to use the real remote server.
    teardown
    launch_local_server

    # WHEN a request is made to the local pollutant server
    call_local_server "/pollutant-rss?latitude=48.8439104&longitude=2.3570831"

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

#!/usr/bin/env bats
# shellcheck shell=bash

load "../../../support/test-actions.bash"

@test "Test prevair nominal pollutant data" {
    # GIVEN a remote pollutant server running which returns valid data
    # AND a local pollutant server waiting for a request
    # WHEN a request is made to the local pollutant server
    # THEN the local pollutant server should return a valid RSS feed

    # GIVEN a remote pollutant server running which returns valid data
    # AND a local pollutant server waiting for a request
    launch_remote_server "pollutant/prevair/ok"

    # WHEN a request is made to the local pollutant server
    call_local_server "/pollutant-rss/prevair?latitude=48.8439104&longitude=2.3570831"

    # THEN the local pollen server should return a valid RSS feed
    [ "${http_status}" -eq 200 ]
    run compare_response "pollutant/prevair/ok"
    [ "$status" -eq 0 ]
}

@test "Test prevair nominal pollutant data with HEAD" {
    # GIVEN a remote pollutant server running which returns valid data
    # AND a local pollutant server waiting for a request
    # WHEN a request using HEAD is made to the local pollutant server
    # THEN the local pollutant server should return a valid HEAD response

    # GIVEN a remote pollutant server running which returns valid data
    # AND a local pollutant server waiting for a request
    launch_remote_server "pollutant/prevair/ok"

    # WHEN a request using HEAD is made to the local pollutant server
    call_local_server "/pollutant-rss/prevair?latitude=48.8439104&longitude=2.3570831" "--head"

    # THEN the local pollen server should return a valid HEAD response
    [ "${http_status}" -eq 200 ]
    run compare_head_response "pollutant/prevair/ok"
    [ "$status" -eq 0 ]
}

@test "Test prevair some invalid pollutant data" {
    # GIVEN a remote pollutant server running which returns some valid data
    # AND a local pollutant server waiting for a request
    # WHEN a request is made to the local pollutant server
    # THEN the local pollutant server should return a valid RSS feed
    # AND the RSS feed should contain only the valid data

    # GIVEN a remote pollutant server running which returns some valid data
    # AND a local pollutant server waiting for a request
    launch_remote_server "pollutant/prevair/some-invalid-data"

    # WHEN a request is made to the local pollutant server
    call_local_server "/pollutant-rss/prevair?latitude=48.8439104&longitude=2.3570831"

    # THEN the local pollutant server should return a valid RSS feed
    # AND the RSS feed should contain only the valid data
    [ "${http_status}" -eq 200 ]
    run compare_response "pollutant/prevair/some-invalid-data"
    [ "$status" -eq 0 ]
}

@test "Test prevair all invalid pollutant data" {
    # GIVEN a remote pollutant server running which returns all invalid data
    # AND a local pollutant server waiting for a request
    # WHEN a request is made to the local pollutant server
    # THEN the local pollutant server should return a valid RSS feed
    # AND the RSS feed should contain no entry

    # GIVEN a remote pollutant server running which returns all invalid data
    # AND a local pollutant server waiting for a request
    launch_remote_server "pollutant/prevair/all-invalid-data"

    # WHEN a request is made to the local pollutant server
    call_local_server "/pollutant-rss/prevair?latitude=48.8439104&longitude=2.3570831"

    # THEN the local pollutant server should return a valid RSS feed
    # AND the RSS feed should contain no entry
    [ "${http_status}" -eq 200 ]
    run compare_response "pollutant/prevair/all-invalid-data"
    [ "$status" -eq 0 ]
}

@test "Test prevair all missing pollutant data" {
    # GIVEN a remote pollutant server running which returns all missing data
    # AND a local pollutant server waiting for a request
    # WHEN a request is made to the local pollutant server
    # THEN the local pollutant server should return a valid RSS feed
    # AND the RSS feed should contain no entry

    # GIVEN a remote pollutant server running which returns all missing data
    # AND a local pollutant server waiting for a request
    launch_remote_server "pollutant/prevair/all-missing-data"

    # WHEN a request is made to the local pollutant server
    call_local_server "/pollutant-rss/prevair?latitude=48.8439104&longitude=2.3570831"

    # THEN the local pollutant server should return a valid RSS feed
    # AND the RSS feed should contain no entry
    [ "${http_status}" -eq 200 ]
    run compare_response "pollutant/prevair/all-missing-data"
    [ "$status" -eq 0 ]
}

@test "Test real prevair server pollutant data" {
    # GIVEN a remote pollutant server running which returns valid data
    # AND a local pollutant server waiting for a request
    # WHEN a request is made to the local pollutant server
    # THEN the local pollutant server should return a valid RSS feed

    # Stop the local pollutant server. We need to restart it configured
    # to use the real remote server.
    teardown
    launch_local_server

    # WHEN a request is made to the local pollutant server
    call_local_server "/pollutant-rss/prevair?latitude=48.8439104&longitude=2.3570831"

    # THEN the local pollutant server should return a valid RSS feed
    [ "$http_status" -eq 200 ]
    # Spot check a couple of values in the response:
    o3_index_count=$(grep -c "O&#8323;: " \
        "${test_log_folder}/actual-local-response-body.txt")
    [ "${o3_index_count}" -eq 1 ]

    no2_index_count=$(grep -c "NO&#8322;: " \
        "${test_log_folder}/actual-local-response-body.txt")
    [ "${no2_index_count}" -eq 1 ]
}

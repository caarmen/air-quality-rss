#!/usr/bin/env bats
# shellcheck shell=bash

load "../support/test-actions.bash"

@test "Test nominal pollen data" {
    # GIVEN a remote pollen server running which returns a valid JSON response
    # AND a local pollen server waiting for a request
    # WHEN a request is made to the local pollen server
    # THEN the local pollen server should return a valid RSS feed

    # GIVEN a remote pollen server running which returns a valid JSON response
    # AND a local pollen server waiting for a request
    launch_remote_server "ok"

    # WHEN a request is made to the local pollen server
    call_local_server "latitude=48.8439104&longitude=2.3570831"

    # THEN the local pollen server should return a valid RSS feed
    [ "$http_status" -eq 200 ]
    run compare_response "ok"
    [ "$status" -eq 0 ]
}

@test "Test real server pollen data" {
    # GIVEN a remote pollen server running which returns a valid JSON response
    # AND a local pollen server waiting for a request
    # WHEN a request is made to the local pollen server
    # THEN the local pollen server should return a valid RSS feed

    # Stop the local pollen server. We need to restart it configured
    # to use the real remote server.
    teardown
    launch_local_server

    # WHEN a request is made to the local pollen server
    call_local_server "latitude=48.8439104&longitude=2.3570831"

    # THEN the local pollen server should return a valid RSS feed
    [ "$http_status" -eq 200 ]
    # Spot check a couple of values in the response:
    pollen_resp_count=$(grep -c "Pollen responsable:" \
        "${test_log_folder}/actual-local-response-body.txt")
    [ "${pollen_resp_count}" -eq 1 ]

    bouleau_count=$(grep -c "Bouleau: " \
        "${test_log_folder}/actual-local-response-body.txt")
    [ "${bouleau_count}" -eq 1 ]
}
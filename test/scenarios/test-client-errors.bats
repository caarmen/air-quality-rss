#!/usr/bin/env bats
# shellcheck shell=bash

load "../support/test-actions.bash"

@test "Test missing latitude" {
    # GIVEN a local pollen server waiting for a request
    # WHEN a request is made to the local pollen server without a latitude query parameter
    # THEN the local pollen server should return the expected error.

    # WHEN a request is made to the local pollen server
    call_local_server "longitude=2.3570831"

    # THEN the local pollen server should return the expected error.
    [ "$http_status" -eq 400 ]
    run compare_response "missing-latitude"
    [ "$status" -eq 0 ]
}

@test "Test missing longitude" {
    # GIVEN a local pollen server waiting for a request
    # WHEN a request is made to the local pollen server without a longitude query parameter
    # THEN the local pollen server should return the expected error.

    # WHEN a request is made to the local pollen server
    call_local_server "latitude=48.8439104"

    # THEN the local pollen server should return the expected error.
    [ "$http_status" -eq 400 ]
    run compare_response "missing-longitude"
    [ "$status" -eq 0 ]
}

@test "Test missing latitude and longitude" {
    # GIVEN a local pollen server waiting for a request
    # WHEN a request is made to the local pollen server without a latitude or longitude query parameter
    # THEN the local pollen server should return the expected error.

    # WHEN a request is made to the local pollen server
    call_local_server ""

    # THEN the local pollen server should return the expected error.
    [ "$http_status" -eq 400 ]
    run compare_response "missing-latitude-longitude"
    [ "$status" -eq 0 ]
}

#!/usr/bin/env bats
# shellcheck shell=bash

load "../../support/test-actions.bash"

@test "Test missing latitude" {
    # GIVEN a local pollen server waiting for a request
    # WHEN a request is made to the local pollen server without a latitude query parameter
    # THEN the local pollen server should return the expected error.

    # WHEN a request is made to the local pollen server
    call_local_server "/pollen-rss?longitude=2.3570831"

    # THEN the local pollen server should return the expected error.
    [ "$http_status" -eq 400 ]
    run compare_response "api/missing-latitude"
    [ "$status" -eq 0 ]
}

@test "Test missing longitude" {
    # GIVEN a local pollutant server waiting for a request
    # WHEN a request is made to the local pollutant server without a longitude query parameter
    # THEN the local pollutant server should return the expected error.

    # WHEN a request is made to the local pollutant server
    call_local_server "/pollutant-rss/prevair?latitude=48.8439104"

    # THEN the local pollutant server should return the expected error.
    [ "$http_status" -eq 400 ]
    run compare_response "api/missing-longitude"
    [ "$status" -eq 0 ]
}

@test "Test missing latitude and longitude" {
    # GIVEN a local pollen server waiting for a request
    # WHEN a request is made to the local pollen server without a latitude or longitude query parameter
    # THEN the local pollen server should return the expected error.

    # WHEN a request is made to the local pollen server
    call_local_server "/pollen-rss?"

    # THEN the local pollen server should return the expected error.
    [ "$http_status" -eq 400 ]
    run compare_response "api/missing-latitude-longitude"
    [ "$status" -eq 0 ]
}

@test "Test unknown route" {
    # GIVEN a local pollen server waiting for a request
    # WHEN a request is made to the local pollen server with an unknown route
    # THEN the local pollen server should return a 404 error.

    # WHEN a request is made to the local pollen server
    call_local_server "/unknown-route?latitude=48.8439104&longitude=2.3570831"

    # THEN the local pollen server should return a 404 error.
    [ "$http_status" -eq 404 ]
    run compare_response "api/unknown-route"
    [ "$status" -eq 0 ]
}

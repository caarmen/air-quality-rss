#!/usr/bin/env bats
# shellcheck shell=bash

load "../../../support/test-actions.bash"

@test "Test atmo france tabular 500 error" {
    # GIVEN a remote pollutant server running which returns a 500 error
    # AND a local pollutant server waiting for a request
    # WHEN a request is made to the local pollutant server
    # THEN the local pollutant server should return a valid RSS feed

    # GIVEN a remote pollutant server running which returns a 500 error
    # AND a local pollutant server waiting for a request
    launch_remote_server "pollutant/atmo-france/tabular/remote-server-500-error"

    # WHEN a request is made to the local pollutant server
    call_local_server "/pollutant-rss/atmo-france/tabular?code_zone=75056"

    # THEN the local pollutant server should return a valid RSS feed
    [ "${http_status}" -eq 200 ]
    run compare_response "pollutant/atmo-france/tabular/remote-server-500-error"
    [ "$status" -eq 0 ]
}

@test "Test atmo france tabular slow" {
    # GIVEN a remote pollutant server running which is extremely slow to respond
    # AND a local pollutant server waiting for a request
    # WHEN a request is made to the local pollutant server
    # THEN the local pollutant server should return a valid RSS feed

    # GIVEN a remote pollutant server running which returns a 500 error
    # AND a local pollutant server waiting for a request
    launch_remote_server "pollutant/atmo-france/tabular/remote-server-slow"

    # WHEN a request is made to the local pollutant server
    call_local_server "/pollutant-rss/atmo-france/tabular?code_zone=75056"

    # THEN the local pollutant server should return a valid RSS feed
    [ "${http_status}" -eq 200 ]
    run compare_response "pollutant/atmo-france/tabular/remote-server-slow"
    [ "$status" -eq 0 ]
}
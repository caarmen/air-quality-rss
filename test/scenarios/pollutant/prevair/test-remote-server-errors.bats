#!/usr/bin/env bats
# shellcheck shell=bash

load "../../../support/test-actions.bash"

@test "Test prevair 500 error" {
    # GIVEN a remote pollutant server running which returns a 500 error
    # AND a local pollutant server waiting for a request
    # WHEN a request is made to the local pollutant server
    # THEN the local pollutant server should return a valid RSS feed

    # GIVEN a remote pollutant server running which returns a 500 error
    # AND a local pollutant server waiting for a request
    launch_remote_server "pollutant/prevair/remote-server-500-error"

    # WHEN a request is made to the local pollutant server
    call_local_server "/pollutant-rss/prevair?latitude=48.8439104&longitude=2.3570831"

    # THEN the local pollen server should return a valid RSS feed
    [ "${http_status}" -eq 200 ]
    run compare_response "pollutant/prevair/remote-server-500-error"
    [ "$status" -eq 0 ]
}
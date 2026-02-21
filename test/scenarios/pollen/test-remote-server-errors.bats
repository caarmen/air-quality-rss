#!/usr/bin/env bats
# shellcheck shell=bash

load "../../support/test-actions.bash"

@test "Test remote server error with 200 status code" {
    # GIVEN a remote pollen server running which returns a 200 error
    # AND a local pollen server waiting for a request
    # WHEN a request is made to the local pollen server
    # THEN the local pollen server should return the expected error.

    # GIVEN a remote pollen server running which returns a 500 error
    launch_remote_server "pollen/remote-server-200-error"

    # WHEN a request is made to the local pollen server
    call_local_server "/pollen-rss?latitude=48.8439104&longitude=2.3570831"

    # THEN the local pollen server should return the expected error.
    [ "$http_status" -eq 500 ]
    run compare_response "pollen/remote-server-200-error"
    [ "$status" -eq 0 ]
}

@test "Test remote server error with 500 status code" {
    # GIVEN a remote pollen server running which returns a 500 error
    # AND a local pollen server waiting for a request
    # WHEN a request is made to the local pollen server
    # THEN the local pollen server should return the expected error.

    # GIVEN a remote pollen server running which returns a 500 error
    launch_remote_server "pollen/remote-server-500-error"

    # WHEN a request is made to the local pollen server
    call_local_server "/pollen-rss?latitude=48.8439104&longitude=2.3570831"

    # THEN the local pollen server should return the expected error.
    [ "$http_status" -eq 500 ]
    run compare_response "pollen/remote-server-500-error"
    [ "$status" -eq 0 ]
}

@test "Test remote server offline" {
    # GIVEN a remote pollen server is offline
    # AND a local pollen server waiting for a request
    # WHEN a request is made to the local pollen server
    # THEN the local pollen server should return the expected error.

    # GIVEN a remote pollen server is offline

    # WHEN a request is made to the local pollen server
    call_local_server "/pollen-rss?latitude=48.8439104&longitude=2.3570831"

    # THEN the local pollen server should return the expected error.
    [ "$http_status" -eq 500 ]
    run compare_response "pollen/remote-server-offline"
    [ "$status" -eq 0 ]
}

@test "Test timeout" {
    # GIVEN a remote pollen server running which is extremely slow to respond
    # AND a local pollen server waiting for a request
    # WHEN a request is made to the local pollen server
    # THEN the local pollen server should return the expected error

    # GIVEN a remote pollen server running which is extremely slow to respond
    launch_remote_server "pollen/remote-server-slow"

    # WHEN a request is made to the local pollen server
    call_local_server "/pollen-rss?latitude=48.8439104&longitude=2.3570831"

    # THEN the local pollen server should return the expected error
    [ "$http_status" -eq 500 ]
    run compare_response "pollen/remote-server-slow"
    [ "$status" -eq 0 ]

}

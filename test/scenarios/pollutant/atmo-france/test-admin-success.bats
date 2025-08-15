#!/usr/bin/env bats
# shellcheck shell=bash

load "../../../support/test-actions.bash"

@test "Test atmo france admin nominal pollutant data" {
    # GIVEN a remote pollutant server running which returns valid data
    # AND a local pollutant server waiting for a request
    # WHEN a request is made to the local pollutant server
    # THEN the local pollutant server should return a valid RSS feed

    # GIVEN a remote pollutant server running which returns valid data
    # AND a local pollutant server waiting for a request
    launch_remote_server "pollutant/atmo-france/admin/ok"

    # WHEN a request is made to the local pollutant server
    call_local_server "/pollutant-rss/atmo-france/admin?code_zone=75056"

    # THEN the local pollen server should return a valid RSS feed
    [ "${http_status}" -eq 200 ]
    run compare_response "pollutant/atmo-france/admin/ok"
    [ "$status" -eq 0 ]
}

@test "Test atmo france admin some missing pollutant data" {
    # GIVEN a remote pollutant server running which returns some missing data
    # AND a local pollutant server waiting for a request
    # WHEN a request is made to the local pollutant server
    # THEN the local pollutant server should return a valid RSS feed
    # AND the RSS feed should contain only the valid data

    # GIVEN a remote pollutant server running which returns some valid data
    # AND a local pollutant server waiting for a request
    launch_remote_server "pollutant/atmo-france/admin/some-missing-data"

    # WHEN a request is made to the local pollutant server
    call_local_server "/pollutant-rss/atmo-france/admin?code_zone=75056"

    # THEN the local pollutant server should return a valid RSS feed
    # AND the RSS feed should contain only the valid data
    [ "${http_status}" -eq 200 ]
    run compare_response "pollutant/atmo-france/admin/some-missing-data"
    [ "$status" -eq 0 ]
}

@test "Test atmo france admin all missing pollutant data" {
    # GIVEN a remote pollutant server running which returns all missing data
    # AND a local pollutant server waiting for a request
    # WHEN a request is made to the local pollutant server
    # THEN the local pollutant server should return a valid RSS feed
    # AND the RSS feed should contain no entry

    # GIVEN a remote pollutant server running which returns all missing data
    # AND a local pollutant server waiting for a request
    launch_remote_server "pollutant/atmo-france/admin/all-missing-data"

    # WHEN a request is made to the local pollutant server
    call_local_server "/pollutant-rss/atmo-france/admin?code_zone=75056"

    # THEN the local pollutant server should return a valid RSS feed
    # AND the RSS feed should contain no entry
    [ "${http_status}" -eq 200 ]
    run compare_response "pollutant/atmo-france/admin/all-missing-data"
    [ "$status" -eq 0 ]
}
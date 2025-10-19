#!/usr/bin/env bash
# shellcheck shell=bash

fixture_folder="test/fixtures"
docker_container_id="" # Set in setup() and used in teardown()
test_log_folder="" # Set in setup()

setup() {
    test_log_folder="logs/${BATS_TEST_NAME}"
    mkdir -p "${test_log_folder}"
    launch_local_server \
        "http://host.docker.internal:8000/ows" \
        "http://host.docker.internal:8000/metadata.json"
}
teardown() {
    echo "# Stopping servers..." >&3
    jobs -p | xargs --no-run-if-empty kill
    docker kill "${docker_container_id}"
}

function launch_local_server() {
    pollen_base_url=$1
    pollutant_metadata_url=$2
    # Make a dummy environment shell script.
    # For tests, we'll pass the environment variables to docker with -e
    touch /tmp/.env.test.sh
    # Start the local air quality server.
    #
    # The --add-host argument is to make `host.docker.internal` available inside the container.
    # This is needed to connect to the local server from inside the container.
    # https://medium.com/@TimvanBaarsen/how-to-connect-to-the-docker-host-from-inside-a-docker-container-112b4c71bc66
    docker_container_id=$(docker run --rm -p 8888:8888 \
        -v /etc/localtime:/etc/localtime:ro \
        -v /tmp/prevair:/tmp/prevair:ro \
        -v /tmp/.env.test.sh:/app/.env.sh:ro \
        -e ATMO_FRANCE_USERNAME="${ATMO_FRANCE_USERNAME}" \
        -e ATMO_FRANCE_PASSWORD="${ATMO_FRANCE_PASSWORD}" \
        -e POLLEN_BASE_URL="${pollen_base_url}" \
        -e POLLUTANT_METADATA_URL="${pollutant_metadata_url}" \
        -e BASE_FEED_URL="http://localhost:8888" \
        --detach \
        --add-host=host.docker.internal:host-gateway \
        air-quality-rss)
    docker logs -f "${docker_container_id}" > "${test_log_folder}/air-quality-server.log" 2>&1 &
    wait_for_text_in_file \
        "${test_log_folder}/air-quality-server.log" \
        "Air quality server started"
}

# Function to launch the remote server.
# Arguments:
#   $1: path to the fixture folder.
function launch_remote_server() {
    fixture_name=$1
    status_code=$(cat \
        "${fixture_folder}/${fixture_name}/mock-remote-response-status-code.txt"\
    )
    replace_date_placeholders \
        "${fixture_folder}/${fixture_name}/mock-remote-response-body.txt" \
        "${test_log_folder}/mock-remote-response-body.txt"
    # Copy any pollutant files to the /tmp/prevair folder
    rm -f /tmp/prevair/*.nc
    cp "${fixture_folder}/${fixture_name}"/*.nc /tmp/prevair/ 2>/dev/null || true
    for file in /tmp/prevair/*__TODAY_SHORT__*; do
        [ -e "${file}" ] || continue
        new_name="${file/__TODAY_SHORT__/$today_short}"
        mv "$file" "$new_name"
    done
    # Start the remote server
    node test/mockserver/mockserver.mjs \
        "${test_log_folder}/mock-remote-response-body.txt" \
        "${status_code}" > "${test_log_folder}/mockserver.log" 2>&1 &
    wait_for_text_in_file \
        "${test_log_folder}/mockserver.log" \
        "listening"
}

function wait_for_text_in_file() {
    # Wait for a string to appear in a file.
    # Arguments:
    #   $1: path to the file to check
    #   $2: string to wait for
    file_path=$1
    search_string=$2
    echo "# Waiting for '${search_string}' in ${file_path}..." >&3
    for i in {1..100}
    do
        if grep --quiet "${search_string}" "${file_path}"
        then
            echo "# Found '${search_string}' in ${file_path}" >&3
            return 0
        fi
        sleep 0.1
    done
    echo "# Failed to find '${search_string}' in ${file_path}" >&3
    return 1
}

# Function to call the local server.
# Arguments:
#   $1: query string to pass to the local server
#   $2: (optional) additional curl options
# Saves the response body and metadata to actual-response-body.txt 
# and actual-response-metadata.json
# and the http status code to the variable http_status.
function call_local_server() {
    request_uri=$1
    additional_curl_options=$2
    # Call the local server
    curl \
        ${additional_curl_options:+$additional_curl_options} \
        --silent \
        --output "${test_log_folder}/actual-local-response-body.txt" \
        --write-out "%{json}" \
        "http://localhost:8888${request_uri}" \
        >  "${test_log_folder}/actual-local-response-metadata.json"
    http_status=$(jq -r '.http_code' \
        "${test_log_folder}/actual-local-response-metadata.json"\
    )
    export http_status
    echo "# response metadata:" >&3
    sed -e 's/^/# /g' "${test_log_folder}/actual-local-response-metadata.json" >&3
    echo "# response body:" >&3
    sed -e 's/^/# /g' "${test_log_folder}/actual-local-response-body.txt" >&3
}

function compare_response() {
    fixture_name=$1
    replace_date_placeholders \
        "${fixture_folder}/${fixture_name}/expected-local-response-body.txt" \
        "${test_log_folder}/expected-local-response-body.txt"
    diff_content=$(diff \
        "${test_log_folder}/expected-local-response-body.txt" \
        "${test_log_folder}/actual-local-response-body.txt"\
    )
    diff_status=$?
    if [ "${diff_status}" -ne 0 ]
    then
        echo "# Actual response different from expected response:" >&3
        # shellcheck disable=SC2001
        # (could be a good suggestion if it weren't multiline)
        echo "${diff_content}" | sed -e 's/^/# /g' >&3
    fi
    exit ${diff_status}
}

function compare_head_response() {
    get_fixture_name=$1
    replace_date_placeholders \
        "${fixture_folder}/${get_fixture_name}/expected-local-response-body.txt" \
        "${test_log_folder}/expected-get-response-body.txt"

    # Get the expected Content-Length from the fixture for the get http method.
    # (xargs trims whitespace.)
    expected_response_content_length=$(wc -c < "${test_log_folder}/expected-get-response-body.txt" | xargs)

    # Check that the Content-Length header matches the expected length.
    grep -i "^Content-Length: ${expected_response_content_length}\s*$" \
        "${test_log_folder}/actual-local-response-body.txt" \
        > /dev/null

    grep_status=$?
    if [ "${grep_status}" -ne 0 ]
    then
        echo "# Actual HEAD response different from expected response:" >&3
        echo "# Expected Content-Length: ${expected_response_content_length}" >&3
        echo "# Actual Content:" >&3
        cat "${test_log_folder}/actual-local-response-body.txt" >&3
    fi
    exit ${grep_status}
}

function replace_date_placeholders() {
    input_file=$1
    output_file=$2
    today=$(date +%Y-%m-%d)
    today_short=$(date +%Y%m%d)
    sed -e \
        "s/__TODAY__/${today}/;s/__TODAY_SHORT__/${today_short}/" \
        "${input_file}" \
        > "${output_file}"
}

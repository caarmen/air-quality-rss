#!/usr/bin/env bash
# shellcheck shell=bash
setup_suite() {
    rm -rf logs
    rm -f ./*.nc
    mkdir -p /tmp/prevair
    echo "# Building the Docker image..." >&3
    docker build -t air-quality-rss .
}

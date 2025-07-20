#!/usr/bin/env bash
# shellcheck shell=bash
setup_suite() {
    rm -rf logs
    echo "# Building the Docker image..." >&3
    docker build -t air-quality-rss .
}

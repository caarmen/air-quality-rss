#!/usr/bin/env bash

if [ $# -ne 1 ]
then
    echo "Usage: $0 <number of iterations>"
    exit 1
fi

number_iterations="$1"

for ((i=0; i<number_iterations; i++))
do
    echo "Iteration $i">&2
    curl -X HEAD -s "http://localhost:8888/pollutant-rss/atmo-france/admin?code_zone=69123"
    curl -s "http://localhost:8888/pollutant-rss/atmo-france/admin?code_zone=69123"
    curl -s "http://localhost:8888/pollutant-rss/atmo-france/admin?code_zone=69123"

    curl -X HEAD -s "http://localhost:8888/pollen-rss?latitude=45.7658125&longitude=4.861183"
    curl -s "http://localhost:8888/pollen-rss?latitude=45.7658125&longitude=4.861183"
    curl -s "http://localhost:8888/pollen-rss?latitude=45.7658125&longitude=4.861183"
done

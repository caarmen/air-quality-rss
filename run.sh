#!/usr/bin/env bash
docker rm airqualfix
docker run \
   --name airqualfix \
   -e BASE_FEED_URL=http://localhost:8888 \
   -e POLLUTANT_METADATA_URL=http://host.docker.internal:9999/pollutant/response.json \
   -e POLLEN_BASE_URL=http://host.docker.internal:9999/pollen/response.json \
   -v $(pwd)/.env.sh:/app/.env.sh:ro \
   -p 8888:8888 \
   -it --entrypoint /bin/bash \
   --cap-add=SYS_PTRACE --security-opt seccomp=unconfined \
   airqual:latest

#   --memory=1000m \
#   --memory-swap=1000m \

#!/bin/bash -e

env="-e DATABASE_URL=postgres://some-url"
img=test-img
container_name=test-container

docker run -it --rm --name $container_name $env $img

# or using host network:
# docker run -it --network=host --rm --name $container_name $env $img

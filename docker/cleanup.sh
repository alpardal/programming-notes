#!/bin/bash

set -e

# file usage:
docker system df
# volume sizes:
docker system df -v

# cleaning up used space, removes:
#   - all stopped containers
#   - all networks not used by at least one container
#   - all dangling images
#   - all build cache
# docker system prune


# Or individual entries:
#  docker image prune
#  docker container prune
#  docker volume prune
#  docker network prune

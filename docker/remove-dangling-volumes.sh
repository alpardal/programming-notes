#!/bin/bash -e

cd $(sudo readlink /var/lib/docker/volumes)
rm -rf $(docker volume ls -qf dangling=true)

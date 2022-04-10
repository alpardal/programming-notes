#!/bin/bash -e

img=test-image

docker build -t $img --file Dockerfile .

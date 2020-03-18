#!/bin/bash

# Assumes you have an x server (like xQuartz) already installed
# and that the "Allow connections from network clients" option is activated
# ref https://medium.com/@mreichelt/how-to-show-x11-windows-within-docker-on-mac-50759f4b65cb
xhost + 127.0.0.1 # add 127.0.0.1 to x server acl
docker run -ti -e DISPLAY=host.docker.internal:0 pennsive/rtapas:latest

#!/bin/sh

if [ "$1" = "build" ]
then
    docker build . -t sheronw1174/sos-env
elif [ "$1" = "pull" ]
then
    docker pull sheronw1174/sos-env
else
    echo "usage: $0 [build|pull]"
fi
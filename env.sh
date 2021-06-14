#!/bin/sh

LOCAL=$HOME/.local

system_repository=$(chicken-install -repository)
binary_version=${system_repository##*/}
local_repository=${LOCAL}/lib/chicken/${binary_version}

export CHICKEN_REPOSITORY_PATH=${system_repository}:${local_repository}
export CHICKEN_INSTALL_REPOSITORY=${local_repository}
export CHICKEN_INSTALL_PREFIX=${LOCAL}

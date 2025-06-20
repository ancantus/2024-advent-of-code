#!/bin/bash

# Constants 
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

# User Inputs
PROJECT="${1}"

# Main Body:
pushd "${SCRIPT_DIR}" > /dev/null 2>&1
make format
make build
popd > /dev/null 2>&1

exec "${SCRIPT_DIR}/_build/default/${PROJECT}/${PROJECT}.exe" ${@:2}

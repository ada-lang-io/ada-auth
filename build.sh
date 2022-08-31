#!/bin/bash

set -euo pipefail

alr build
pushd source/
mkdir -p output/
../.obj/arm_form aa-aarm.msm Ada_Lang_IO New-Changes 5
popd

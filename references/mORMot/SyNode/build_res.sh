#!/bin/sh

rm -rf ./.resources
./tools/core_res -i ./core_modules/ -o ./.resources/
x86_64-w64-mingw32-windres ./.resources/core_res.rc ./core_modules.res
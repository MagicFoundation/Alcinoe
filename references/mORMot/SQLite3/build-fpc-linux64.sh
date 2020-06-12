#!/bin/bash

rm -rf ./fpc/*
mkdir -p ./fpc/lib/x86_64-linux
mkdir -p ./fpc/bin/x86_64-linux

fpc -B -MObjFPC -Scagi -Cg -Cirot -gw2 -gl -l -dFPCSQLITE3STATIC -dUseCThreads -Fi -Fifpc/lib/x86_64-linux -Fl./fpc-linux64 -Fu./SQLite3 -Fu./SQLite3/DDD/dom -Fu./SQLite3/DDD/infra "-Fu./SQLite3/Samples/33 - ECC" -Fu. -FUfpc/lib/x86_64-linux -FEfpc/bin/x86_64-linux/ ./SQLite3/TestSQL3.dpr

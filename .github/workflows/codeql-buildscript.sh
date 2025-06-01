#!/usr/bin/env bash

./configure LDFLAGS="-Wl,-rpath=. -L. -ltcc" "CFLAGS= -s -O2" --full --with-ext=tcc
make

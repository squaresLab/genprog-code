#!/bin/bash

export CC="gcc --save-temps"
cd wireshark
make -k
cd ..
#!/bin/bash

export CC="gcc -save-temps"
cd python
make -k
if [ $? -ne 0 ]
then
    make clean
    make -k
    if [ $? -ne 0 ]
    then
        make distclean
        ./configure --prefix=/home/claire/local-root
        make
    fi
fi
cd ..
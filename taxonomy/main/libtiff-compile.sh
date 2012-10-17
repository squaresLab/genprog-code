#!/bin/bash

export CC="gcc -save-temps"
cd libtiff
make -k
if [ $? -ne 0 ]
then
    make clean
    make -k 
    if [ $? -ne 0 ]
    then
        sh autogen.sh 
        ./configure --prefix=/home/claire/local-root
        make -k
    fi
fi
cd ..
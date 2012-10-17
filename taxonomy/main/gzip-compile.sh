#!/bin/bash

export CC="gcc -save-temps"
cd gzip
make -k
if [ $? -ne 0 ]
then
    make clean
    make -k 
    if [ $? -ne 0 ]
    then
        ./buildconf
        ./configure -prefix=/home/claire/local-root
        make -k
    fi
fi
cd ..
#!/bin/bash

export CC="gcc -save-temps"
pushd php
make -k
if [ $? -ne 0 ]
then
    make clean
    make -k
    if [ $? -ne 0 ]
    then
        make distclean
        ./buildconf
        ./configure --prefix=/home/claire/local-root
        make -k
    fi
fi

popd
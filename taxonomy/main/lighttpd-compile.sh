#!/bin/bash

export CC="gcc -save-temps"
cd lighttpd
cd src
num=`wc -l settings.h | cut -f 1 -d ' '`
less=$(( $num - 2 ))
head -$less settings.h > temp.h
echo "#define __FILE__ \"foo\"" >> temp.h
echo "#define __LINE__ 5" >> temp.h
echo "#endif" >> temp.h
mv temp.h settings.h
cd ..
make
cd ..
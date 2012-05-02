#!/bin/bash


# $1 = EXE
# $2 = SOURCE

cp $2 ./src/gcd.c
cd ./src/gcd.c

gcc-4.0 -o ../$1 gcd.c main.c

cd ..
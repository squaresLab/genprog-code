#!/bin/bash

#I just need a script that copies repair.sanity.txt to a file with a .c extension so that gcc stops acting like a whiny bitch just because the file extension is slightly unexpected. This script does so. You will note that the config files have the compiler flag set as:
#--compiler ./txt2c.sh
#This is why.

ExeName=$2
SourceName=$3
CompilerOptions=$4

cp $SourceName temporary.c

gcc -o $ExeName temporary.c $CompilerOptions


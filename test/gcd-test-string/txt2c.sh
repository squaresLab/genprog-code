#!/bin/bash

#I just need a script that copies repair.sanity.txt to a file with a .c extension so that gcc stops acting like a whiny bitch just because the file extension is slightly unexpected. This script does so. You will note that the config files have the compiler flag set as:
#--compiler ./txt2c.sh
#This is why.

ExeName=$1
shift
SourceName=$1
shift

TempName=`mktemp --suffix .c`
cp $SourceName $TempName

gcc -o $ExeName $TempName "$@"
status=$?

rm -f $TempName
exit $status

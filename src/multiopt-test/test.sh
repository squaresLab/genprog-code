#!/bin/bash
# $1 = EXE 
# $2 = test name  
# $3 = port 
# $4 = source name
# $5 = single-fitness-file name 
# exit 0 = success
ulimit -t 5
case $2 in
  p1) $1 | diff output - && exit 0 ;;

  s) # fitness 
  $1 > test.tmp
  echo `cat $1 | wc -c`, > $5
  ./difference `cat test.tmp` `cat output` >> $5
  rm test.tmp 
  exit 1 
  ;;

esac 
exit 1

#!/bin/bash
# $1 = EXE 
# $2 = test name  
# $3 = port 
# $4 = source name
# $5 = single-fitness-file name 
# exit 0 = success
ulimit -t 1
echo $1 $2 $3 $4 $5 >> testruns.txt
case $2 in
  p1) $1 1071 1029 | diff output.1071.1029 - && exit 0 ;;
  p2) $1 555 666 | diff output.555.666 - && exit 0 ;;
  p3) $1 678 987 | diff output.678.987 - && exit 0 ;;
  p4) $1 8767 653 | diff output.8767.653 - && exit 0 ;;
  p5) $1 16777216 512 | diff output.16777216.512 - && exit 0 ;;
  n1) $1 0 55 | diff output.0.55 - && exit 0 ;;

  s) # single-valued fitness 
  let fit=0
  $1 1071 1029 | diff output.1071.1029 - && let fit=$fit+1
  $1 555 666 | diff output.555.666 - && let fit=$fit+1
  $1 678 987 | diff output.678.987 - && let fit=$fit+1
  $1 8767 653 | diff output.8767.653 - && let fit=$fit+1
  $1 16777216 512 | diff output.16777216.512 - && let fit=$fit+1
  ($1 0 55 | diff output.0.55 -) && let fit=$fit+1
  let passed_all_so_stop_search="$fit >= 6"
  echo $fit > $5
  if [ $passed_all_so_stop_search -eq 1 ] ; then 
    exit 0 
  else
    exit 1 
  fi ;;


esac 
exit 1

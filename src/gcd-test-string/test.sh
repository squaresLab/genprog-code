#!/bin/sh
# $1 = EXE 
# $2 = test name  
# $3 = port 
# exit 0 = success
ulimit -t 1
case $2 in
  p1) $1 1071 1029 | diff output.1071.1029 - && exit 0 ;;
  p2) $1 555 666 | diff output.555.666 - && exit 0 ;;
  p3) $1 678 987 | diff output.678.987 - && exit 0 ;;
  p4) $1 8767 653 | diff output.8767.653 - && exit 0 ;;
  p5) $1 16777216 512 | diff output.16777216.512 - && exit 0 ;;
  n1) $1 0 55 | diff output.0.55 - && exit 0 ;;
esac 
exit 1

#!/bin/sh
# $1 = EXE 
# $2 = test name  
# $3 = port 
# exit 0 = success
ulimit -t 1
cd `dirname $1`
case $2 in
  p1) java gcd 1071 1029 | diff -w ../output.1071.1029 - && exit 0 ;;
  p2) java gcd 555 666 | diff -w ../output.555.666 - && exit 0 ;;
  p3) java gcd 678 987 | diff -w ../output.678.987 - && exit 0 ;;
  p4) java gcd 8767 653 | diff -w ../output.8767.653 - && exit 0 ;;
  p5) java gcd 16777216 512 | diff -w ../output.16777216.512 - && exit 0 ;;
  n1) java gcd 0 55 > res && diff -w res ../output.0.55 && exit 0 ;;
esac 
exit 1

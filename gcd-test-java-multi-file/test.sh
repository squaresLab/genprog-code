#!/bin/sh
# $1 = EXE 
# $2 = test name  
# $3 = port 
# exit 0 = success
ulimit -t 1
cd `dirname $1`
case $2 in
  #sample
  p1) java -cp . gcdp.GCD 1071 1029 | diff -w ../output.1071.1029 - && exit 0 ;;
  p2) java -cp . gcdp.GCD 555 666 | diff -w ../output.555.666 - && exit 0 ;;
  p3) java -cp . gcdp.GCD 678 987 | diff -w ../output.678.987 - && exit 0 ;;
  p4) java -cp . gcdp.GCD 8767 653 | diff -w ../output.8767.653 - && exit 0 ;;
  p5) java -cp . gcdp.GCD 16777216 512 | diff -w ../output.16777216.512 - && exit 0 ;;
  n1) java -cp . gcdp.GCD 0 55 > res && diff -w res ../output.0.55 && exit 0 ;;
esac 
exit 1

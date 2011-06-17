#!/bin/sh
# $1 = path to cobertura.jar
# $2 = path to instrumented class(es)
# $3 = path to un-instrumented class(es)
# $4 = path to data file
# $5 = test name
# exit 0 = success

cd `dirname $3`
case $5 in
  p1) java -cp $1:$2:$3 -Dnet.sourceforge.cobertura.datafile=$4 gcd 1071 1029 | diff -w output.1071.1029 - && exit 0 ;;
  p2) java -cp $1:$2:$3 -Dnet.sourceforge.cobertura.datafile=$4 gcd 555 666 | diff -w output.555.666 - && exit 0 ;;
  p3) java -cp $1:$2:$3 -Dnet.sourceforge.cobertura.datafile=$4 gcd 678 987 | diff -w output.678.987 - && exit 0 ;;
  p4) java -cp $1:$2:$3 -Dnet.sourceforge.cobertura.datafile=$4 gcd 8767 653 | diff -w output.8767.653 - && exit 0 ;;
  p5) java -cp $1:$2:$3 -Dnet.sourceforge.cobertura.datafile=$4 gcd 16777216 512 | diff -w output.16777216.512 - && exit 0 ;;
  n1) java -cp $1:$2:$3 -Dnet.sourceforge.cobertura.datafile=$4 gcd 0 55
      exit 1 ;;
esac 
exit 1

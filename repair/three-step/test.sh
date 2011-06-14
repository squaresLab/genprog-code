#!/bin/sh
# $1 = EXE 
# $2 = test name  
# $3 = port 
# exit 0 = success
ulimit -t 1
case $2 in
  p1) $1 1 | diff o.1 - && exit 0;;
  p2) $1 2 | diff o.2 - && exit 0;;
  p3) $1 3 | diff o.3 - && exit 0;;
  p4) $1 4 | diff o.4 - && exit 0;;
  p5) $1 5 | diff o.5 - && exit 0;;
  n1) $1 666 | diff o.666 - && exit 0 ;;
esac 
exit 1

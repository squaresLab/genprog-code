#!/bin/sh
ulimit -t 2
rm -rf tmp 
case $2 in
	p1) $1 -z testfile.c -c | diff goodtest.c.bz2 - && exit 0 ;;
	p2) $1 -z testfile.c -s -c | diff goodtestsmall.c.bz2 - && exit 0 ;;
	p3) $1 -z testfile.c -1 -c | diff goodtest1.c.bz2 - && exit 0 ;;
	p4) $1 -z testfile.c -2 -c | diff goodtest2.c.bz2 - && exit 0 ;;
	p5) $1 -z testfile.c -3 -c | diff goodtest3.c.bz2 - && exit 0 ;;
	p6) $1 -z testfile.c -4 -c | diff goodtest4.c.bz2 - && exit 0 ;;
	p7) $1 -z testfile.c -5 -c | diff goodtest5.c.bz2 - && exit 0 ;;
	p8) $1 -z testfile.c -6 -c | diff goodtest6.c.bz2 - && exit 0 ;;
	p9) $1 -z testfile.c -7 -c | diff goodtest7.c.bz2 - && exit 0 ;;
	p10) $1 -z testfile.c -8 -c | diff goodtest8.c.bz2 - && exit 0 ;;
	p11) $1 -z testfile.c -9 -c | diff goodtest9.c.bz2 - && exit 0 ;;
	p12) $1 -z empty -c | diff emptytest.bz2 - && exit 0 ;;
	n1) exit 1 ;;
esac
exit 1

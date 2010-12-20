#!/bin/sh
ulimit -t 2
rm -rf my.tp1 my.tp2 my.tp3 my.tp4 my.tp5
case $2 in
				p1) $1 test/test.vy | grep "Unnamed" - | wc -l | diff pass - && exit 0 ;;
				n1) $1 test/test.vy && exit 1 ;;
esac
exit 1

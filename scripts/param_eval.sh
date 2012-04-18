#!/bin/sh

for i in 0 0.01 1.0; do
    for j in 0.03 0.06 0.09; do
	rm -rf [0-9]*
	../../ga/modify --good_path_factor $i --mut $j $1 >& /dev/null
	mv $1.debug debug-$i-$j.txt
    done
done
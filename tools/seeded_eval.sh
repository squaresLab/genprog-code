#!/bin/sh

for ((k = 0; k < 50; k++))
do
    rm -rf [0-9]*
    ../../ga/modify --good_path_factor 0.01 --mut 0.06 --seed $k $1 >& /dev/null
    mv $1.debug seeded_debug-$k-0.01-0.06.txt
    if [ ! -f $1-best.c ]
    then
	../../ga/modify --good_path_factor 0.00 --mut 0.03 --seed $k $1 >& /dev/null
	mv $1.debug seeded_debug-$k-0.00-0.03.txt
    fi
    touch $1-best.c
    mv $1-best.c $1-$k-best.c
done
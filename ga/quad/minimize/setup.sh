#!/bin/sh
# requires: best.c baseline.c
date > minimized.log
../../cdiff --generate mymin baseline.c best.c 
delta -suffix=.diff -test=./test-diff -cp_minimal=min.diff < mymin.diff 
../../cdiff --use mymin min.diff > minimized.c
date >> minimized.log

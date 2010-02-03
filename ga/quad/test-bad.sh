#!/bin/sh
ulimit -t 1
$1 0 55 | diff output.0.55 - && (echo "0 55" >> $2)
exit 0

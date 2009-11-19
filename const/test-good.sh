#!/bin/bash
ulimit -t 1
($1 0 | diff output.0 - && (echo "0" >> $2)) &
($1 3 | diff output.3 - && (echo "3" >> $2)) &
($1 6 | diff output.6 - && (echo "6" >> $2)) &
($1 9 | diff output.9 - && (echo "9" >> $2)) &
wait
exit 0

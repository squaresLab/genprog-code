#!/bin/bash
ulimit -t 1
$1 15 | diff output.15 - && (echo "15" >> $2)
exit 0

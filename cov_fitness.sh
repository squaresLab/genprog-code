#!/bin/sh
#
# Script for performing and writing out coverity output
# $1 = variant name 

if [ $# -ne 1 ]
then
echo "given: ./cov_fitness.sh $*"
echo "usage: ./cov_fitness.sh source_file.c"
exit 0
fi

arg1=$1
len=${#arg1}
len=`expr $len-2`
proj=${arg1:0:$len}
echo "Proj: |$proj|"

# get coverity fitness
cd /home/zpf5a/workspace/geneticAlgs/coverityOut/$proj/
rm -rf analysis-dir
./run.sh
cd analysis-dir/c/output/
curline=`grep "New defects found" summary.txt`
echo "$curline" >tempCurLine.txt
cut -f 2 -d ":" tempCurLine.txt | cut -f 2 -d " " >/home/zpf5a/workspace/geneticAlgs/genprog-code/trunk/repair/fitness.txt
rm tempCurLine.txt
cd /home/zpf5a/workspace/geneticAlgs/genprog-code/trunk/repair
#cat fitness.txt

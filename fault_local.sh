#!/bin/bash

export PATH_TO_INSTR=/Users/csl9q/research/genprog/code-cbi/datapredict/
export PATH_TO_REPAIR=/Users/csl9q/research/genprog/code-cbi/repair

export SRC=$1
export POSTESTS=$2
export NEGTESTS=$3
export GCC=$4
export RAND=$5
# TODO: will need to add ldflags or similar

function generate_schemes () 
{
    echo "context failure importance increase intersection0.01 intersection00 uniform"

    for (( i = 1; i <= $RAND ; i ++ )) 
    do
	echo "random"$i" "
    done
}

function random_file_move ()
{
    local fname=$1
    local suff=$2

    if [ -f $fname ]
    then
	fbase=`basename $fname "$2"`
	RANGE=1000
	number=$RANDOM
	let "number %= $RANGE"

    while [ -f $fname".save"$number$suff ] 
    do
	number=$RANDOM
	let "number %= $RANGE"
    done
    echo $fname" already in this directory; moving to "$fbase".save"$number$suff"..."
    mv $fname $fbase".save"$number$suff
    fi
}

function run_tests() 
{
    local NUMTESTS=$1
    local TESTPRE=$2 
    local PASSEDORFAILED=$3

    for (( i = 1; i <= $NUMTESTS ; i ++ ))
    do
	./test.sh "./"$basename"-instr" $TESTPRE$i
	mv $SRC".preds" $SRC".preds"$TESTPRE$i
	echo $SRC".preds"$TESTPRE$i" "$PASSEDORFAILED >> preds_file.txt
    done
}

function make_results_dir ()
{
    local STRAT=$1
    if [ ! -d "results/"$STRAT ]
    then
	mkdir "results/"$STRAT
	mkdir "results/"$STRAT"/debug"
	mkdir "results/"$STRAT"/repairs"
    fi
}

REPAIR_FOUND=0
REPAIR_NOT_FOUND=1

# start by getting the basename of the program in question

basename=`basename $SRC ".c"`
echo "BASENAME: "$basename 

function call_repair ()
{
    local SCHEME=$1
    local fault_local=$2
    local SEED=$3
    local MUT=$4
	local FIX=$5
    local weight_file=$basename"-"$fault_local"-fault_local.txt"

    $PATH_TO_REPAIR/repair --fix-scheme $FIX --search $SCHEME  --mutp $MUT --no-rep-cache --program $SRC --pos-tests $POSTESTS --neg-tests $NEGTESTS --seed $seed --predict-input $SRC".sites" --use-weight-file $weight_file --compiler $GCC
    mv "repair.debug."$seed "results/"$SCHEME"/debug/debug__fault_"$fault_local"__fix_"$FIX"__mut"$MUT"__seed"$SEED".debug"
    if [ -f repair.c ] 
    then 
	mv repair.c "results/"$SCHEME"/repairs/repair__fault_"$fault_local"__fix_"$FIX"__mut"$MUT"__seed"$SEED".c"
	return $REPAIR_FOUND
    else
	return $REPAIR_NOT_FOUND
    fi
}


# instrument and compile the code for predicate generation
$PATH_TO_INSTR/instrument --default $SRC > $basename"_instr.c"

$GCC -o $basename"-instr" $basename"_instr.c"

# make preds_file, generate predicates on test cases
if [ -f preds_file.txt ] ; then rm preds_file.txt ; fi
touch preds_file.txt

run_tests $POSTESTS "p" "PASSED"
run_tests $NEGTESTS "n" "FAILED"

# generate fault localization files 
$PATH_TO_INSTR/predict -name $basename -rs preds_file.txt -rand $RAND -cbi-hin $SRC".sites" -inter 0.01 -inter 0.00  >& $basename".predict_debug.txt"

# run repair a hundred different ways on the different localization files produced above

if [ ! -d results ] ; then mkdir results ; fi
random_file_move repair.c ".c"

# the following does brute force search for each localization strategy, and
# several times for random (since we make several of those)

make_results_dir "brute"
make_results_dir "ga"

seed=0

for fault_local in $(generate_schemes)
do 
	for fix in "default" "uniform"
	do
		call_repair "brute" $fault_local $seed 0.01 $fix
	done
done

# now, ga for each fault localization strategy.  

for fault_local in $(generate_schemes)
do 
    for (( seed=0; seed<5;seed++ ))
    do
		for fix in "default" "uniform"
		do
			for mutp in 0.01 0.06
			do
				call_repair "ga" $fault_local $seed $mutp
				return_val=$? 
				if [ $return_val -eq $REPAIR_FOUND ]
				then
					break
				fi
			done
		done
    done
done
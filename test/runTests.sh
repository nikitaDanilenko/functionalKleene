#!/bin/bash

sizes=(0100 0250 0500 0750 1000)
densities=( 0.001 0.01 0.025 0.05 0.1 )
generators=(-1673289139 1483475230 -825569446)
functions=( array left right block )
types=( b txb t )
SEP="-------------------------------------------------------------------------------"
FILE=$1

# The function requires one parameter, namely the file name where to write all the results.
# It then applies the main function runTest to every possible combination of semirings (types),
# sizes, densities, generators and functions listed above.

for t in ${types[@]}
    do for s in ${sizes[@]}
        do for d in ${densities[@]}
            do for g in ${generators[@]}
                do for f in ${functions[@]}
                    do echo "runTest" $s $d $g $t $f
                       echo "runTest" $s $d $g $t $f >> $FILE
                       ./runTest $s $d $g $t $f +RTS -t >> $FILE 2>&1
                       echo $SEP >> $FILE
                    done
                done
            done
        done
    done
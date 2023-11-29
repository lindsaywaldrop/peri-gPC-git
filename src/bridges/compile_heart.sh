#!/bin/bash

#Shell script for compiling the main2d program in IBAMR. To be run on Bridges.
WD=${1:?Please provide a path for the main directory}
track=${2:?Please provide a circulatory system}
#a=${3:?Please provide a number of simulations to run}
#name=${4:?Please provide your bridges username}

cd "$WD"
echo "Setting up directories..."
mkdir bin/
mkdir results/
mkdir results/ibamr
mkdir results/ibamr/runs/
mkdir results/ibamr/log-files/
mkdir results/visit/
mkdir results/visit/${track}/
mkdir results/r-csv-files/
mkdir results/r-csv-files/${track}/
mkdir data/input2d-files/
mkdir data/parameters-files/
mkdir data/csv-files/
mkdir data/csv-files/${track}/
mkdir data/ibamr-files/
mkdir data/ibamr-files/${track}/

cd "$WD"/bin
echo "Compiling main2d..."
cp "$WD"/src/ibamr/* .
make main2d
rm *.o stamp-2d *.C Makefile *.h

cd "$WD"/src/bridges/

#sh setinput2d.sh "$WD" ${a}
#sh setparameters.sh "$WD" ${a}


echo "Complete"

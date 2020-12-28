#!/bin/bash

WD=${1:?Please provide a path for the main directory}
track=${2:?Please provide a circulatory system}
a=${3:?Please provide a number of simulations to run}

cd "${WD}"/results/ibamr/${track}_runs/

cut -f 3 "$WD"/data/parameters/allpara_${a}.txt > Freq.txt

echo "viz_IB2d files:"
for i in `seq 1 $a`; do

  Freq=$(awk -v var="$i" 'NR==var' "Freq.txt")
  
  if [ $(echo " $Freq <= 0.70" | bc) -eq 1 ]; then
    if [ -d viz_IB2d${i}/lag_data.cycle_350000 ]; then
      echo "Simulation found" 
    else 
      echo "Simulation ${i} not found"
    fi
  elif [ $(echo " $Freq < 1.0" | bc) -eq 1 ]; then
    if [ -d viz_IB2d${i}/lag_data.cycle_300000 ]; then
      echo "Simulation found" 
    else 
      echo "Simulation ${i} not found"
    fi
  else
    if [ -d viz_IB2d${i}/lag_data.cycle_250000 ]; then
      echo "Simulation found"
    else 
      echo "Simulation ${i} not found"
    fi
  fi
done

echo "hier_data_IB2d files:"
for i in `seq 1 $a`; do

  Freq=$(awk -v var="$i" 'NR==var' "Freq.txt")
  
  if [ $(echo " $Freq <= 0.70" | bc) -eq 1 ]; then
    if [ -f hier_data_IB2d${i}/F.350000 ]; then
      echo "Simulation found" 
    else 
      echo "Simulation ${i} not found"
    fi
  elif [ $(echo " $Freq < 1.0" | bc) -eq 1 ]; then
    if [ -f hier_data_IB2d${i}/F.300000 ]; then
      echo "Simulation found" 
    else 
      echo "Simulation ${i} not found"
    fi
  else
    if [ -f hier_data_IB2d${i}/F.250000 ]; then
      echo "Simulation found"
    else 
      echo "Simulation ${i} not found"
    fi
  fi
done
rm Freq.txt
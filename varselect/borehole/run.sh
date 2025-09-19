#!/bin/bash

for (( seed=1; seed<=1; seed++ ))
do
    R CMD BATCH "--args seed=$seed" borehole.R &
done

 

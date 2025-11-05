#!/bin/bash

for (( seed=4; seed<=10; seed++ ))
do
    R CMD BATCH "--args seed=$seed" borehole.R &
done

 

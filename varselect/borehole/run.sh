#!/bin/bash

for (( seed=41; seed<=50; seed++ ))
do
    R CMD BATCH "--args seed=$seed" borehole.R &
done

 

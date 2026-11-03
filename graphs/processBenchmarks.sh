#! /bin/sh
mlr --csv put '$K = substr(regextract($Name, "K=[0-9]*"),2, -1)' divnBench.csv |
mlr --csv put '$N = substr(regextract($Name, "N=[0-9]*"),2, -1)' |
mlr --csv put '$Q = substr(regextract($Name, "Q=[0-9]*"),2, -1)' |
mlr --csv put '$Experiment = substr(regextract($Name, "Experiment=[0-9]*"), 11, -1)' |
mlr --csv put '$File = substr(regextract($Name, "File=[^,]*,"), 5, -2)' |
mlr --csv put '$Settings = substr(regextract($Name, "Name=[^,]*,"),5, -2)' |
mlr --csv cut -x -f Name > partiallyProcessed.csv

mlr --csv filter '$Experiment == "1"' partiallyProcessed.csv |
mlr --csv cut -x -f Experiment > benchExperiment1.csv

mlr --csv filter '$Experiment == "2"' partiallyProcessed.csv |
mlr --csv cut -x -f Experiment > benchExperiment2.csv

# gnuplot -e "filename='quickProcessed.csv'; testgroup = 'bsort'" -p test.gnuplot;

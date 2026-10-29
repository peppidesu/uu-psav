#! /usr/bin/bash
mlr --csv put '$K = substr(regextract($Name, "K=[0-9]*"),2, -1)' divnBench.csv |
mlr --csv put '$settings = substr(regextract($Name, "settings=.*"),9, -1)' |
mlr --csv put '$file = substr(regextract($Name, "name=.*N"), 5, -3)' |
mlr --csv put '$N = substr(regextract($Name, "N=[0-9]*"),2, -1)' |
mlr --csv put '$Q = substr(regextract($Name, "Q=[0-9]*"),2, -1)' |
mlr --csv put '$experiment = substr(regextract($Name, "experiment=[0-9]*"), 11, -1)' |
mlr --csv rename Mean,mean,MeanLB,meanLB,MeanUB,meanUB,Stddev,stddev,StddevUB,stddevUB,StddevLB,stddevLB |
mlr --csv cut -x -f Name > quickProcessed.csv

mlr --csv filter '$experiment == "1"' quickProcessed.csv |
mlr --csv cut -x -f experiment,N > benchExperiment1.csv

mlr --csv filter '$experiment == "2"' quickProcessed.csv |
mlr --csv cut -x -f experiment > benchExperiment2.csv

# gnuplot -e "filename='quickProcessed.csv'; testgroup = 'bsort'" -p test.gnuplot;
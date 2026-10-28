#! /usr/bin/bash
mlr --csv put '$K = substr(regextract($Name, "K=[0-9]*"),2, -1)' quick.csv |
mlr --csv put '$Settings = substr(regextract($Name, "settings=.*"),9, -1)' |
mlr --csv put '$File = substr(regextract($Name, "name=.*N"), 5, -2)' |
mlr --csv put '$N = substr(regextract($Name, "N=[0-9]*"),2, -1)' |
mlr --csv cut -x -f Name > quickProcessed.csv

gnuplot -e "filename='quickProcessed.csv'; testgroup = 'bsort'" -p test.gnuplot;
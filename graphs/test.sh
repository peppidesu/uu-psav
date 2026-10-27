#! /usr/bin/bash
mlr --csv put '$K = substr(regextract($Name, "K = [0-9]*"),3, -1)' benchFixed.csv |
mlr --csv put '$Settings = substr(regextract(regextract($Name, "K = [0-9]*.*"),"/.*"),1, -1)' |
mlr --csv put '$File = substr(regextract($Name, "[^/]*/N ="), 0, -5)' |
mlr --csv put '$N = substr(regextract($Name, "N = [0-9]*"),3, -1)' |
mlr --csv cut -x -f Name > benchFixedProcessed.csv

gnuplot -e "filename='benchFixedProcessed.csv'; testgroup = 'bsort'" -p test.gnuplot;
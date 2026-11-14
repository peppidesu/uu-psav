#! /bin/sh
mlr --csv cut -f N,wasValid stats.csv |
mlr --csv count-distinct -g N,wasValid |
mlr --csv filter '$wasValid == "False"' |
mlr --csv cut -x -f wasValid > killed.csv
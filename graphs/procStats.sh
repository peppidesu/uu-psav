#! /usr/bin/bash
mlr --csv label cfg_1,cfg_2,cfg_3,cfg_4,cfg_5,cfg_6,cfg_7,cfg_8 stats.csv |
mlr --csv nest --implode --values --across-fields -f cfg |
# possibly sub cfg fields
mlr --csv put '$File = substr(regextract($fileName, "[^/]*.gcl"), 0, -5)' |
mlr --csv put '$RunGroup = regextract($File, "[^_]*")' |
mlr --csv gsub -f RunGroup invalidD d then gsub -f RunGroup invalidB b then gsub -f RunGroup invalidF f then gsub -f RunGroup invalidM m then gsub -f RunGroup invalidP p |
mlr --csv rename cfgK,K |
mlr --csv gsub -f cfg "False;True;True;True;True;True;True;True" "simplifier enabled" then sub -f cfg "False;False;True;True;True;True;True;True" "simplifier disabled"|
mlr --csv cut -x -f throwaway,fileName > statsProc.csv
mlr --csv filter '$Settings == "no simplifier"' benchFixedProcessed.csv |
mlr --csv cut -x -f Settings,MeanLB,MeanUB,Stddev,StddevLB,StddevUB > benchFixedProcessedUnsimpl.csv

mlr --csv filter '$Settings != "no simplifier"' benchFixedProcessed.csv |
mlr --csv cut -x -f MeanLB,MeanUB,Stddev,StddevLB,StddevUB |
mlr --csv join -f benchFixedProcessedUnsimpl.csv -j File,N,K --lp default |
mlr --csv put '$RunGroup = regextract($File, "[^_]*")' |
mlr --csv gsub -f RunGroup invalidD d then gsub -f RunGroup invalidB b then gsub -f RunGroup invalidF f then gsub -f RunGroup invalidM m then gsub -f RunGroup invalidP p |
mlr --csv put '$Percentage = ($Mean / $defaultMean)*100' > benchPercentageOfDefault.csv
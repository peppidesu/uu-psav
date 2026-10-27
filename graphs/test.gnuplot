set datafile separator ","
set title testgroup
plot filename using (column("K")):(column("Mean")) with linespoints title 'Default settings' if ((strcol("Settings") eq "default settings") && (strcol("File") eq testgroup)),\
     filename using (column("K")):(column("Mean")) with linespoints title 'No simplifier' if ((strcol("Settings") eq "no simplifier") && (strcol("File") eq testgroup))
#, 'benchFixed.csv' using 1:3 with lines
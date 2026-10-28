set datafile separator ","
set jitter overlap 1.5 swarm
set xrange [10:38]
set xtics 16
plot 'benchPercentageOfDefault.csv' using (column("K")):(column("Percentage")):(column("K")) with points lt rgb 'red' if (strcol("RunGroup") eq "bsort"),\
     'benchPercentageOfDefault.csv' using (column("K")):(column("Percentage")):(column("K")) with points lt rgb 'green' if (strcol("RunGroup") eq "divByN"),\
     'benchPercentageOfDefault.csv' using (column("K")):(column("Percentage")):(column("K")) with points lt rgb 'blue' if (strcol("RunGroup") eq "find12"),\
     'benchPercentageOfDefault.csv' using (column("K")):(column("Percentage")):(column("K")) with points lt rgb 'orange' if (strcol("RunGroup") eq "memberOf"),\
     'benchPercentageOfDefault.csv' using (column("K")):(column("Percentage")):(column("K")) with points lt rgb 'yellow' if (strcol("RunGroup") eq "min"),\
     'benchPercentageOfDefault.csv' using (column("K")):(column("Percentage")):(column("K")) with points lt rgb 'purple' if (strcol("RunGroup") eq "pullUp"),



# From: https://stackoverflow.com/questions/53659762/how-to-plot-x-y-z-points-showing-their-density
# set terminal gif animate delay 20
# set output "SO53659762.gif"
#     a0     = 40
#     a1     = 60
#     Frames = 24
#     do for [i=0:Frames] {
#         set view 65,(sin(2*pi*i/(Frames+1))*(a1-a0)+a0)
#         replot
#     }
# set output
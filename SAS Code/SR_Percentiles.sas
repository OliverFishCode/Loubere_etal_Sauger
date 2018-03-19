Data waterSRmean;
Input value Site$;
Datalines;
2.2500 OHR
2.5300 OHR

"Insert your data
in place of the restricted
datalines I have provided"

1.68 MMR
1.65 MMR
;
proc sort data=waterSRmean;
by site;
run;
Proc univariate data=waterSRmean normal ;
var value;
by site;
run;
quit;

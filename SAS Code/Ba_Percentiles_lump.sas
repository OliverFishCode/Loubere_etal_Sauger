Data waterBAmean;
Input value Site$;
Datalines;
0.3200 OHRTRB
0.3400 OHRTRB

"Insert your data
in place of the restricted
datalines I have provided"

0.4600 MMR
0.4600 MMR
;
proc sort data=waterBAmean;
by site;
run;
Proc univariate data=waterBAmean normal;
var value;
by site;
run;
quit;

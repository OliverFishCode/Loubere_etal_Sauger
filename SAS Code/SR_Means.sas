data srmeans;
input value site$;
datalines;
2.2500 OHR
2.5300 OHR

"Insert your data
in place of the restricted
datalines I have provided"

1.68 MMR
1.65 MMR
;
proc means data=srmeans mean;
class site ;
run;

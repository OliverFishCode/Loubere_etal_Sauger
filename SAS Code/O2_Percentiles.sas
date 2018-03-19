Data water02mean;
Input value Site$;
Datalines;
-6.396582762 OHR
-6.966168446 OHR

"Insert your data
in place of the restricted
datalines I have provided"

-6.76 TRB
-8.45 TRB
;
proc sort data=water02mean;
by site;
run;
Proc univariate data=water02mean normal ;
var value;
by site;
run;
quit;

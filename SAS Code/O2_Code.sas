data waterO;
input value site$;
datalines;
-5.8200 OHR
-6.3500 OHR

"Insert your data
in place of the restricted
datalines I have provided"

-7.0200 GRR
-6.7600 GRR
;
proc glm data=waterO;
class site;
model value = site;
lsmeans site /  cl lines adjust=Tukey;
output out=resids p=pred r=resid;
proc print;
goptions reset;
symbol value=dot;
proc gplot;
plot pred*resid;
run;
proc univariate noprint data=resids;
qqplot resid / normal;
run;
quit;

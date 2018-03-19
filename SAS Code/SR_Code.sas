data watersr;
input value site$;
datalines;
0.3522 OHR
0.4031 OHR

"Insert your data
in place of the restricted
datalines I have provided"

0.225309282 MMR
0.217483944 MMR
;
proc glm data=watersr;
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
